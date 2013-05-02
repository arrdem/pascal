(def testcode
  '(progn (:= a (+ 1 5))
          (:= b (* a 10))
          (:= c (* b a))))

;; ## X86 Floating Point
;; x86 does floats on stack machine, the top of which is the register %st(0), or
;; simply %st. %st(1) is the first item down, %st(2) the 2nd and soforth. This
;; stack goes down to %st(7).

;;## Reserved Registers:
;;   rpb is out
;;   rsp is out
;;   all other x86_64 used tho.
(def teststate
  {:registers {}
   :memory {'a 0x00 'b 0x04 'c 0x08}
   :free-registers ["%rax" "%rbx" "%rcx" "%rdx" "%rsi" "%rdi" "%r8" "%r9"
                    "%r10" "%r11" "%r12" "%r13" "%r14" "%r15"]
   :types {'a "integer" 'b "integer" 'c "integer"}
   })

(defn typeof [state sym]
  (get-in state [:types sym]))

(defn genstore
  "Generates a store operation, moving one symbol to its memory address
   implicitly freeing the register for other uses. Only returns the store
   operation."
  [state sym]
  (case (typeof state sym)
    ("integer")
        (format "    mov -%s(%%rbp), %s ; store %s to data seg\n"
                (get-in state [:memory sym])
                (get-in state [:registers sym])
                (name sym))
    ("real" "float")
        (format "   movl -%s(%%rbp), %%st(0) ; store %s to data seg\n"
                (get-in state [:memory sym])
                (name sym))))

(defn genload
  "Generates a load operation which brings a symbol from its memory address to
   the designated register. Does not return an updated symbol table, just
   returns the load operation. Note that floats are always loaded to %st(0)."
  [state sym dst]
  (if (number? sym)
    (format "$%X" sym)
    (case (typeof state sym)
      ("integer")
          (format "    mov %s, -%s(%%rbp) ; load %s from data seg\n"
                  dst
                  (get-in state [:memory sym])
                  (name sym))
      ("real" "float")
          (format "    fldl -%s(%%rbp) ; load %s to the top of the float stack"
                  (get-in state [:memory sym])
                  (name sym)))))

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

(defn maybe-allocate-register
  "Returns a tripple (state, code, register) being (state, nil, <register>) if
   the argument symbol was already allocated, or else (new-state, <code>,
   <dst-register>) where code is some sequence of load and store operations
   which result in the desired symbol being present in the register named by
   dst-register. Note that <code> will be _empty_ not nil if there was an open
   register and spilling was not required."
  [state sym]
  (cond
   ;; constant case
   (number? sym)
     (list state nil (format "$0x%X" sym))

   ;; already allocated case
   (get-in state [:registers sym])
     (list state
           nil
           (get-in state [:registers sym]))

   ;; need to allocate and there are free regs
   (first (:free-registers state))
     (list (-> state
               (update-in [:free-registers]
                          rest)
               (assoc-in  [:registers sym]
                          (first (:free-registers state))))
           '()
           (first (:free-registers state)))

  ;; need to allocate and there aren't any free
  true
    (let [s (rand-nth (keys (:registers state)))
          r (get-in state [:registers s])]
      (list (-> state
                (update-in [:registers] dissoc s)
                (assoc-in  [:registers sym] r))
            (list (format "                       ; forced to spill symbol %s\n"
                          (name s))
                  (genstore state s))
            r))))

(defn loadsym
  "Loads a symbol to a register, spilling as required and returning a tripple
   (new-state, code, register) where register is the register containing the
   loaded symbol."
  [state sym]
  (let [[new-state ops reg] (maybe-allocate-register state sym)
        loadexpr  (list (genload state sym (first (:free-registers state))))]
    (cond
     (nil? ops) (list state nil reg)
     (empty? ops) (list new-state loadexpr  reg)
     true (list new-state (concat ops loadexpr) reg))))
