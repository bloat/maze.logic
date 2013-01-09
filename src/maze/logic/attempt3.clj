(ns maze.logic.attempt3
  (:refer-clojure :exclude [==]) 
  (:use [clojure.core logic]
        [maze.logic mazes utils]))

(defn path-starts-at-0 [p]
  (firsto p 0))

(defn path-ends-at-end [p mz]
  (let [last-cell (dec (* (first mz) (first mz)))]
    (matche [p]
            ([[last-cell]])
            ([[_ . ?r]] (path-ends-at-end ?r mz)))))

(defne valid-path [p mz]
  ([[] _])
  ([[_] _])
  ([[?h0 ?h1 . ?t] _]
     (fresh [t1]
            (path-in-maze [?h0 ?h1] mz)
            (conso ?h1 ?t t1)
            (valid-path t1 mz))))

(defne no-back-track [p]
  ([[]])
  ([[_]])
  ([[_ _]])
  ([[?h0 ?h1 ?h2 . ?t]]
     (fresh [t1 t2]
            (!= ?h0 ?h2)
            (conso ?h2 ?t t1)
            (conso ?h1 t1 t2)
            (no-back-track t2))))

(defn mz-solve [maze]
  (run 1 [q] 
       (path-starts-at-0 q)
       (path-ends-at-end q maze)
       (valid-path q maze)
       (no-back-track q)))

(comment 
  (mz-solve a-maze2)
  (mz-solve a-maze3)
  (mz-solve a-maze4)
  (mz-solve a-maze5)
  (mz-solve a-maze6)
  ;; After this point its too slow for my computer.
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
