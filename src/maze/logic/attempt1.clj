(ns maze.logic.attempt1
  (:refer-clojure :exclude [==]) 
  (:use [clojure.core logic]
        [maze.logic.mazes]))

(defn path-in-maze [p mz]
  (let [mz-paths (vec (second mz))]
    (fresh [a b]
           (== p [a b])
           (conde
            ((membero [a b] mz-paths))
            ((membero [b a] mz-paths))))))

(defn valid-path [p mz]
  (fresh [h t]
         (conde 
          ;; empty path is valid
          ((emptyo p))
          ;; or everything below
          ((firsto p h)
           (resto p t)
           ;; h (the head of the path) is in the maze, in either order
           (path-in-maze h mz)
           ;; The second cell of h is the first cell of the next entry in the path.
           (conde
            ((emptyo t))
            ((fresh [a b c]
                    (firsto t [a b])
                    (== h [c a]))))
           ;; The rest of the path is valid.
           (valid-path t mz)))))

(defn path-starts-at-0 [p]
  (fresh [h a b]
         (firsto p h)
         (== h [a b])
         (conde
          ((== a 0))
          ((== b 0)))))

(defn path-ends-at-end [p mz]
  (let [last-cell (dec (* (first mz) (first mz)))]
    (fresh [h t]
           (firsto p h)
           (resto p t)
           (conde
            ((fresh [a b]
                    (firsto p h)
                    (resto p t)
                    (emptyo t)
                    (== h [a b])
                    (== b last-cell)))
            ((path-ends-at-end t mz))))))

(defn steps-unequal [s1 s2]
  (fresh [a b]
         (== s2 [a b])
         (!= s1 [a b])
         (!= s1 [b a])))

(defne distincto
  "A relation which guarantees no element of l will unify
with another element of l."
  [l]
  ([()])
  ([[h]])
  ([[h0 h1 . t]]
     (steps-unequal h0 h1)
     (distincto (lcons h0 t))
     (distincto (lcons h1 t))))

(defn mz-solve [maze]
  (run 1 [q] 
        (path-starts-at-0 q)
        (path-ends-at-end q maze)
        (valid-path q maze)
        (distincto q)))

(comment 
  (mz-solve a-maze2)
  (mz-solve a-maze3)
  (mz-solve a-maze4)
  (mz-solve a-maze5)
  (mz-solve a-maze6)
  ;; After this point its too slow for my computer
  (mz-solve a-maze7)
  (mz-solve a-maze8)
  (mz-solve a-maze9)
  (mz-solve a-maze10))

;; This file is part of Maze Logic.

;; Maze Logic is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; Maze Logic is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with Maze Logic. If not, see <http://www.gnu.org/licenses/>.
