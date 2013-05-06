(ns me.arrdem.pascal.symtab.stdmacros
  (:require [me.arrdem.compiler :refer [sizeof nameof]]
            [me.arrdem.compiler.symtab :refer [install! search]]
            [me.arrdem.compiler.macros :refer [->MacroType]]
            [me.arrdem.compiler.ast :refer [makefuncall]]
            [me.arrdem.pascal.semantics :refer [binop]]))

;;------------------------------------------------------------------------------
(defn new-macro
  "A macro function which serves to boostrap the equivalent of a malloc call.
   Takes on argument: a type, and expands to a call to the trnew function which
   actually allocates memory at runtime."
  [[t]]
  (let [T (search t)]
    (assert (not (nil? T)) (str "Failed to find type " t " in the symbol tbl"))
    (assert (not (string? T)) (str "got a string for " t " in the symbol tbl"))
    (list ':= t (makefuncall "new" (list (sizeof T))))))

;;------------------------------------------------------------------------------

;;------------------------------------------------------------------------------
(defn- progn? [form]
  (and (list? form)
       (= (first form) 'progn)))

(defn- _progn-inliner
  [body]
  (reduce (fn [prev form]
            (concat prev
                    (cond
                     (progn? form)
                         (_progn-inliner (rest form))
                     (nil? form)
                         '()
                     true
                      (list form))))
          nil body))

(defn progn-inliner
  "A macro function which serves to try and inline out nested progn groups.
   Derived from
   https://github.com/valeryz/MacroPHP/blob/master/special-forms.lisp#L20"
  [body]
  (if (list? body)
    (cons 'progn (_progn-inliner body))
    body))

;;------------------------------------------------------------------------------
(defn- arith? [expr]
  (and (list? expr)
       (contains? #{'+ '- '* '/ '%} (first expr))))

(defn arith-cleaner [init_ittr init_val op forms]
  (if (list? forms)
    (->> (init_ittr forms)
         (reduce (fn [state x]
                   (cond
                    ;; strip nested additions
                    ;; note that the macro system will take care of the recursive
                    ;; case for me here, I just need  to perform one inlining
                    ;; transform in order to have made progress
                    (and (arith? x)
                         (= op (first x)))
                        (update-in state [:exprs] concat (next x))

                    ;; strip additions of zero
                    (= 0 x)
                        state

                    ;; maintain a partial sum
                    (number? x)
                        (update-in state [:partial] (eval op) x)

                   true
                        (update-in state [:exprs] concat (list x))))
                 {:partial (init_val forms) :exprs '()})
         ((juxt :partial :exprs))
         (apply cons)
         (cons op)
         ((fn [x] (if (= 2 (count x)) (second x) x))))
    forms))

(defn drop-const-prefix-maybe [c op expr]
  (cond
     (not (seq? expr))
       expr
     (= c (second expr))
       (apply list op
              (drop 2 expr))
     true
       expr))


(defn addition-cleaner [forms]
  (->> forms
       (arith-cleaner identity (fn [x] 0) '+)
       (drop-const-prefix-maybe 0 '+)))

(defn multiplication-cleaner [forms]
  (->> forms
       (arith-cleaner identity (fn [x] 1) '*)
       (drop-const-prefix-maybe 1 '*)))

(defn eval-expr-maybe [fn vals]
  (if (every? number? vals)
    (apply fn vals)
    vals))

(defn apply-prefix-maybe [prefix vals]
  (if (seq? vals)
    (cons prefix vals)
    vals))

(defn subtraction-cleaner [forms]
  (->> (list (first forms)
             (addition-cleaner (rest forms)))
       (eval-expr-maybe -)
       (apply-prefix-maybe '-)))

(defn division-cleaner [forms]
  (->> (list (first forms)
             (multiplication-cleaner (rest forms)))
       (eval-expr-maybe /)
       (apply-prefix-maybe '/)))

;;------------------------------------------------------------------------------
(defn aref-cleaner
  "A macro function which seeks first to remove (aref <> 0) groups, and second
   to optimize nested aref statements out to a single aref by taking the sum of
   their index values and eliminating the inner aref."
  ([[expr offset]]
     (cond
      (= 0 offset)
        expr ;; zero offset case
      (= 'aref (first expr))
        (list 'aref
              (second expr)
              (binop offset '+ (nth expr 2)))
      true
        (list 'aref expr offset))))

;;------------------------------------------------------------------------------
(defn init!
  "Function of no arguments, its sole purpose is to side-effect the symbol
   table and install the standard macros used for pre-code generation type
   ensuring and soforth."
  []
  (doseq [m [["new" new-macro]
             ["progn" progn-inliner]
             ["aref" aref-cleaner]
             ["+" addition-cleaner]
             ["*" multiplication-cleaner]
             ["-" subtraction-cleaner]
             ["/" division-cleaner]
             ]]
    (install! (apply ->MacroType m))))
