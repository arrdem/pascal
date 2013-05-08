(ns me.arrdem.compiler.code-generation
  (:require [clojure.set :as s
                         :refer [union difference]]
            [me.arrdem.compiler :refer [addrof typeof]]
            (me.arrdem.compiler.symbols
                 [complex] [core] [records])))

;; # Register State #
;;------------------------------------------------------------------------------
;; The register state is defined to be a map, of the structure
;;
;; {
;;  :free-regs #{}   (type clojure.lang.set)
;;  :used-regs #{}   (type clojure.lang.set)
;;  :preamble  {     (type clojure.lang.map)
;;    :code  ()      (type clojure.lang.list)
;;    :installed #{} (type clojure.lang.set)
;;  }
;; }
;;
;; The idea is that the union of freed-regs and used-regs is constant, being the
;; set of architectural registers.

;;------------------------------------------------------------------------------
;; register state API

(def free-regs :free-regs)
(def used-regs :used-regs)

(defn register?
  "Predicate used to test whether, in the context of the argument state, the
   argument register is in fact a valid register."
  [state reg]
  (contains? (union (free-regs state)
                    (used-regs state))
             reg))

;;------------------------------------------------------------------------------
;; (register) state manipulating functions

(defn- -use-reg
  [state reg]
  (-> state
      (update-in [free-regs] difference (set [reg]))
      (update-in [used-regs] union (set [reg]))))

(defn use-reg
  "Allocates a register, returning a new state record in which the argument
   register is marked as used. May or may not warn if the argument register is
   already marked as used, returns the same state in this case."
  [state register]
  (let [free (free-regs state)
        used (used-regs state)]
    (cond (not (register? state register))
          state

          (contains? free register)
          (-use-reg state register)

          (contains? used register)
          ;; optional warning case
          (-use-reg state register))))

(defn free-reg
  "Deallocates a register, returning a new state record in which the argument
   register is marked as free. May warn if the argument register is already
   marked used."
  [state register]
  (if (register? state register)
    (-> state
        (update-in [free-regs] union (set [register]))
        (update-in [used-regs] difference (set [register])))
    state))

(defn reg-alloc
  "Allocates a register, returning a pair (new-state, register) if there is at
   least one unused register in the argument state. If there are no unused
   registers in the state then a pair (state, nil) is returned."
  [state]
  (let [free (free-regs state)]
    (if (empty? free)
      [state nil]
      [(use-reg state (first free)) (first free)])))

;;------------------------------------------------------------------------------
;; genarith and supporting functions

(declare genc genarith genop genc gendref gensub genmul genadd genfuncall
         genlabel gengoto genftoi genitof loadlit loadsym)

(defn genaddr [state sym-or-expr]
  (cond (string? sym-or-expr)
            (loadlit state (addrof sym-or-expr))

        (list? sym-or-expr)
            (genarith state sym-or-expr)))

(defn genlr
  "Utility function which wraps the normal case of processing arguments in which
   both the left and right hand sides of an expression are treated as
   expressions and computed via genarith."
  [state [lhs rhs]]
  (let [[state lhs-code lhs-dst] (genarith state lhs)
        state (-> state
                  (use-reg lhs-dst))
        [state rhs-code rhs-dst] (genarith state rhs)]
    [state [lhs-code lhs-dst]
           [rhs-code rhs-dst]]))

;; forward declare genarith's helpers...

(defn genarith
  "Fundimental arithmatic expression generator. Operates on typed expressions
   via the me.arrdem.compiler/ITyped protocol. For all cases, returns a tripple
   (new-state, code, save-register). All registers are assumed to be free save
   for the return register. Other registers may be meaningful, but such meaning
   is entirely optional and may be discarded at any time."
  [state expr]
  (cond
   (list? expr)
     (case (first expr)
       (:=) (genc state expr)
       (deref) (gendref state expr)
       (integer->real) (genitof state expr)
       (real->integer) (genftoi state expr)
       (+) (genadd state expr)
       (-) (gensub state expr)
       (*) (genmul state expr))
       ;; note that division and modulus are not implemented.

   (number? expr)
     (loadlit state expr)

   (string? expr)
     (loadsym state expr)
   ))

;;------------------------------------------------------------------------------
;; fragment generators used to simplify the genarith function.

(defn genc
  "Assignment operator generator which treats the left hand side differently,
   being an address generating expression if an expression, or performs an
   address computation (but not a load) if symbolic. Returns a tripple (state,
   code, dst) for dst being constantly nil."
  [state [_assignop lhs rhs]]
  (let [[state lhs-code lhs-dst] (genaddr state lhs)
        state (-> state
                  (use-reg lhs-dst))
        [state rhs-code rhs-dst] (genarith state rhs)]
    [state
     (concat (list (format "    ;; [genc] %s\n" (list := lhs rhs))
                           "    ;; left hand side first..\n")
             lhs-code
             (list         "    ;; right hand side\n")
             rhs-code
             (list (format "    mov (%s), %s ;; write back to target address\n"
                           lhs-dst rhs-dst)))
     nil]))

(defn gendref
  "Deref operation generator. Only takes on argument expression, computing it
   via (genaddr) and emits a single mov instruction to dereference the value
   in the returned register."
  [state [_deref expr]]
  (let [[state code dst] (genaddr state expr)]
    [state
     (concat (list (format "    ;; [genderef] %s\n" (list 'deref expr))
                           "    ;; compute src addr...\n")
             code
             (list         "    ;; deref returned value...\n"
                   (format "    mov (%s), %s\n" dst dst)))
     nil]))

;; declare the math functions in a block since they share a huge ammount of code

(defn genop [iformatstr fformatstr state [op l r]]
  (let [[state coder dstr] (genarith state r)
        state (-> state
                  (use-reg dstr))
        [state codel dstl] (genarith state l)]
    (if (and (= dstr "st(0)")
             (= dstl "st(0)"))
      ;; float case
      [state
       (list fformatstr)
       "st(0)"]

      ;; integer case
      (list (-> state
                (free-reg dstr)
                (use-reg dstl))
            (concat codel
                    coder
                    (list (format iformatstr dstl dstr)))
            dstl))))

(defn genadd [state arg]
  ((partial genop "    add %s, %s\n"
                  "    fadd st(0), st(1)\n")
   state arg))

(defn gensub [state arg]
  ((partial genop "    sub %s, %s\n"
                  "    fsub st(0), st(1)\n")
   state arg))

(defn genmul [state arg]
  ((partial genop "    mul %s, %s\n"
                  "    fmul st(0), st(1)\n")
   state arg))

;; again note that division and modulus are not provided due to not appearing in
;; any of our input cases.

(defn genfuncall
  "Generates a function call for a rather silly calling convention. Single
   argument functions only, taking arguments from the RAX register, returning
   values via the RAX register. If RAX is already used, then we have to swap the
   value of RAX out and preserve it with a temp register, otherwise we just
   clobber RAX and yield RAX as our destination register."
  [state [_funcall fn arg]]
  (let [[state code dst] (genarith state arg)]
    (if (contains? (used-regs state) "%rax")
      ;; rax is used case
      (let [[state tmp] (reg-alloc state)]
        [(-> state (free-reg tmp))
         (concat (list (format "    ;; [genfuncall] %s %s\n"
                               fn arg)
                               "    ;; compute the argument..\n")
                 code
                 (list (format "    mov %s, %%rax ;; stash RAX\n"
                               tmp)
                       (format "    mov %%rax, %s ;; move the arg in place\n"
                               dst)
                       (format "    call %s ;; call it in...\n"
                               fn)
                       (format "    mov %s, %%rax ;; save the result\n"
                               dst)
                       (format "    mov %%rax, %s ;; fix rax\n"
                               tmp)))
         dst])

      ;; rax is unused case
        [(-> state
             (free-reg dst)
             (use-reg "%rax"))
         (concat (list (format "    ;; [genfuncall] %s %s\n"
                               fn arg)
                               "    ;; compute the argument..\n")
                 code
                 (list (format "    mov %%rax, %s ;; move the arg in place\n"
                               dst)
                       (format "    call %s ;; call it in...\n"
                               fn)))
         "%rax"])))

(defn genlabel
  "A simple but essential function for emitting ASM labels"
  [state [_label id]]
  [state
   (list (format "l_%s:\n" id))
   nil])

(defn gengoto
  "A simple but essential function for emitting toto statements"
  [state [_goto id]]
  [state
   (list (format "    jmp l_%s ;; jump to label %s\n"
                 id id))
   nil])

(defn loadlit
  "Generates the appropriate literal loading code for both floating point and
   integer values. Used to encode AST literals in to ASM, always consuming
   either a float or integer register. As with the gen* function series yields
   a (state code dst) tripple."
  [state lit]
  (case (typeof lit)
    ("float" "real") ;; both because my type naming is bad..
        [state
         (list (format "    push %s ;; store to a known memory address\n" lit)
                       "    fld (%rsp) ;; load float from bottom of stack\n"
                       "    add %rsp, 8 ;; knock that value off the stack\n")
         "st(0)"]

    ("integer" nil)
        (let [[state dst] (reg-alloc state)]
          [(-> state
               (free-reg dst))
           (list (format "    mov %s, %s ;; load constant to register\n"
                         dst lit))
           dst])))

(defn loadsym
  "Loads a symbol's value in memory to a register, returning the usual tripple
   for continuity with the other state manipulation functions."
  [state sym-id]
  (case (typeof sym-id)
    ("real" "float")
        (let [addr (addrof sym-id)]
          [state
           (list (format "    fld (%s) ;; load float %s from memory\n"
                         addr sym-id))
           "st(0)"])

    :else
        (let [[state dst] (reg-alloc state)
              addr (addrof sym-id)]
          [(-> state
               (free-reg dst))
           (list (format "    mov %s, (%s) ;; load symbol %s from memory\n"
                         dst addr sym-id))
           dst])))
