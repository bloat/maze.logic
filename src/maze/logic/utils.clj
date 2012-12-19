(ns maze.logic.utils
  (:refer-clojure :exclude [==]) 
  (:use [clojure.core logic]))

(defn path-in-maze [p mz]
  (let [mz-paths (vec (second mz))]
    (fresh [a b]
           (== p [a b])
           (conde
            ((membero [a b] mz-paths))
            ((membero [b a] mz-paths))))))

;; (defne distincto
;;   "A relation which guarantees no element of l will unify
;; with another element of l."
;;   [l]
;;   ([()])
;;   ([[h]])
;;   ([[h0 h1 . t]]
;;      (steps-unequal h0 h1)
;;      (distincto (lcons h0 t))
;;      (distincto (lcons h1 t))))

;; Borrowed distincto from core logic 0.8 beta and 
;; reimplemented it in basic core.logic style.
(defn distincto [p unequal-test]
  (conde
   ;; the empty list is ok
   ((emptyo p))
   ;; a list with only one element is ok
   ((fresh [t]
           (resto p t)
           (emptyo t)))
   ;; otherwise
   ((fresh [h0 h1 t0 t1 a0 a1]
           ;; get the first element into h0
           (firsto p h0)
           ;; get the second element (the first of the rest) into h1
           (resto p t0)
           (firsto t0 h1)
           ;; get the rest of the rest into t1
           (resto t0 t1)
           ;; the first and second elements are unequal
           (unequal-test h0 h1)
           ;; a0 is a list with the first element and t1
           (conso h0 t1 a0)
           ;; a1 is a list with the second element and t1
           (conso h1 t1 a1)
           ;; make sure the two new lists are also distinct
           (distincto a0 unequal-test)
           (distincto a1 unequal-test)))))