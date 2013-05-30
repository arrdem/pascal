(ns me.arrdem.compiler.code-generation.registers
  (:require [clojure.set :as s
                         :refer [union difference]]))

;; # Register State #
;;-----------------------------------------------------------------------------
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

(def x86-regs #{'%rax '%rbx '%rcx '%rdx '%rsi' '%rdi '%r8 '%r9 '%r10
                '%r11 '%r12 '%r13 '%r14 '%r15})

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
