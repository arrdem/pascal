(ns me.arrdem.compiler.codegen
  (:require [clojure.set :as s]
            [me.arrdem.compiler :refer [nameof typeof addrof sizeof]]
            [me.arrdem.compiler.macros :refer [pmacroexpand]]
            [me.arrdem.macros :refer [-<n>]]))

;;------------------------------------------------------------------------------
;; register allocator

;;## Reserved Registers:
;;   rpb is out
;;   rsp is out
;;   all other x86_64 used tho.

(def x86-64-regs
  #{"%rax" "%rbx" "%rcx" "%rdx" "%rsi" "%rdi" "%r8" "%r9" "%r10" "%r11" "%r12"
    "%r13" "%r14" "%r15"})

(defn genstore
  "Generates a store operation, moving one symbol to its memory address
   implicitly freeing the register for other uses. Only returns the store
   operation."
  [state sym]
  (case (typeof sym)
    ("integer")
        (format "    mov -%s(%%rbp), %s ;; store %s to data seg\n"
                (get-in state [:memory sym])
                (get-in state [:registers sym])
                (name sym))
    ("real" "float")
        (format "   movl -%s(%%rbp), %%st(0) ;; store %s to data seg\n"
                (get-in state [:memory sym])
                (name sym))))

(defn genload
  "Generates a load operation which brings a symbol from its memory address to
   the designated register. Does not return an updated symbol table, just
   returns the load operation. Note that floats are always loaded to %st(0)."
  [state sym dst]
  (if (number? sym)
    (format "$%X" sym)
    (case (typeof sym)
      ("integer")
          (format "    mov %s, -%s(%%rbp) ;; load %s from data seg\n"
                  dst
                  (get-in state [:memory sym])
                  (name sym))

      ("real" "float")
          (format "    fldl -%s(%%rbp) ;; load %s to the top of the float stack"
                  (get-in state [:memory sym])
                  (name sym)))))

;; ## Register State
;; The register state contains two values: a set of (free) registers, and a map
;; of symbols to locations. A location is defined to be either a register
;; symbol, or to be an integer being an offset into the heap constituting a load
;; address.
;;
;; Register allocation consists of looking up a symbol in the state table,
;; returning its current register if already live, otherwise allocating a new
;; register generating spill code where required. Returns a tripple being
;; (maybe-updated-state, code, register)

(defn allocate-register [state]
  (-<n> state
         [(update-in <> [:free-registers] (comp set rest))
          nil
          (first (:free-registers <>))]))

(defn allocate-register-for-sym
  "Returns a tripple (state, code, register) being (state, nil, <register>) if
   the argument symbol was already allocated, or else (new-state, <code>,
   <dst-register>) where code is some sequence of load and store operations
   which result in the desired symbol being present in the register named by
   dst-register. Note that <code> will be _empty_ not nil if there was an open
   register and spilling was not required."
  [state sym]
  (cond
   ;; already allocated case
   (get-in state [:registers sym])
     (list state nil (get-in state [:registers sym]))

   ;; need to allocate and there are free regs
   (first (:free-registers state))
     (let [[state _ reg] (allocate-register state)]
       (list (assoc-in state [:registers sym] reg) nil reg))

  ;; need to allocate and there aren't any free
  true
    (let [s (rand-nth (keys (:registers state)))
          r (get-in state [:registers s])
          [state code dst] (allocate-register-for-sym
                            (-> state
                                (update-in [:registers] dissoc s)
                                (update-in [:free-registers] s/union (set [r])))
                            sym)]
      (list state
            (concat
             (list
              (format "    ;; forced to spill symbol %s\n" (name s))
              (genstore state s))
             code)
            dst))))

(defn loadsym
  "Loads a symbol to a register, spilling as required and returning a tripple
   (new-state, code, register) where register is the register containing the
   loaded symbol."
  [state sym]
  (let [[new-state ops reg] (allocate-register-for-sym state sym)
        loadexpr  (list (genload state sym (first (:free-registers state))))]
    (cond
     (nil? ops)
       (list state #{reg}  nil reg)

     (empty? ops)
       (list new-state #{reg} loadexpr reg)

     true
       (list new-state #{reg} (concat ops loadexpr) reg))))

(defn loadlit [state sym]
  (list state nil nil (format "$%x" sym)))

;;------------------------------------------------------------------------------

(defn free-regs [state to-free]
  (update-in state [:free-registers]
             s/difference (set to-free)))

(defn use-reg [state to-use]
  (update-in state [:free-registers]
             s/union state (set [to-use])))

(defn use-regs [state to-use]
  (update-in state [:free-registers]
             s/union state (set to-use)))

;;------------------------------------------------------------------------------

(declare genarith)

(defn genassign [state [op l r]]
  (if (string? l)
    (let [state (assoc state :toplevel op)
          [state usedr coder dstr] (genarith (assoc state :side :rhs) r)]
      (list (-> state
                (free-regs usedr))
            #{}
            (concat coder
                    (list (format "movq -%s(%%rbp) %s\n"
                                  (addrof l) dstr)))
            nil))
    (let [state (assoc state :toplevel op)
          [state usedr coder dstr] (genarith (assoc state :side :rhs) r)
          [state usedl codel dstl] (genarith (assoc state :side :lhs) l)]
      (list (-> state
                (free-regs usedl)
                (free-regs usedr))
            #{}
            (concat codel
                    coder
                  (list (format "movq -%s(%%rbp) %s\n"
                                dstl dstr (str l))))
            nil)))) ;; this actually does use no registers :-p

(defn genop [formatstr state [op l r]]
  (let [[state usedr coder dstr] (genarith state r)
        state (-> state (free-regs usedr)
                  (use-reg dstr))
        [state usedl codel dstl] (genarith state l)]
    (list (-> state
              (free-regs usedl)
              (free-regs usedr)
              (use-reg dstl))
          #{dstl}
          (concat codel
                  coder
                  (list (format formatstr dstl dstr)))
          dstl)))

(defn genadd [state arg]
  ((partial genop "add %s %s\n") state arg))

(defn gensub [state arg]
  ((partial genop "sub %s %s\n") state arg))

(defn genmul [state arg]
  ((partial genop "mul %s %s\n") state arg))

(defn gendiv [state arg]
  ((partial genop "div %s %S\n") state arg))


;; While this is effective I don't need to force the r14 and r15 registers
;; just string format in the two destingation registers and all will be well
(defn genmod [state [op l r]]
  (let [[state usedr coder dstr] (genarith state r)
        state (-> state (free-regs usedr)
                  (use-reg dstr))
        [state usedl codel dstl] (genarith state l)]
    (list (-> state
              (free-regs usedl)
              (use-reg dstl))
          #{dstl}
          (concat codel
                  coder
                  (list "pushq %rdx\n"
                        "movq %%rdx $0 ; EDX has to be zeroed\n"
                        (format "div %s %s" dstl dstr)
                        (format "movq %s %%rdx\n" dstl)
                        "popq %rdx\n"))
          dstl)))

;; ## X86 Floating Point
;; x86 does floats on stack machine, the top of which is the register %st(0), or
;; simply %st. %st(1) is the first item down, %st(2) the 2nd and soforth. This
;; stack goes down to %st(7).

(defn genftoi
  "Generates a float -> integer conversion, taking a float stack register as the
   first argument and an integer register as the second argument."
  []
  )

(defn genitof
  "Generates an integer -> float conversion, which unfortunately involves
   pushing the float stack."
  []
  )

(defn genderef [state [_dref expr]]
  (let [[state usede codee dste] (genarith state expr)]
    (list (-> state
              (free-regs usede)
              (use-reg dste))
          #{dste}
          (concat codee
                  (list (format "movq (%s) %s ;; (dref %s)"
                                dste dste expr))))))

(defn genaref [state [_aref arr ind]]
  (let [[state useda codea dsta] (genarith arr state)
        state (-> state
                  (free-regs useda)
                  (use-reg dsta))
        [state usedi codei dsti] (genarith state ind)]
    (list (-> state
              (free-regs usedi)
              (use-reg dsta))
          #{dsta}
          (concat codea
                  codei
                  (list (format "add-  %s %s\n"
                                dsta dsti)
                        (format "movq %s (%s)\n"
                                dsta dsta))))))

(defn genarith [state expr]
  (cond

   (list? expr)
     (case (first expr)
       (:=) (genassign state expr)
       (aref) (genaref state expr)
       (+) (genadd state expr)
       (-) (gensub state expr)
       (*) (genmul state expr)
       (/) (gendiv state expr)
       (%) (genmod state expr))

   (number? expr)
     (loadlit state expr)

   (string? expr)
     (loadsym state expr)

   ))
