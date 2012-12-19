(ns maze.logic.attempt2
  (:refer-clojure :exclude [==]) 
  (:use [clojure.core logic]
        [maze.logic mazes utils]))

(defn path-starts-at-0 [p]
  (firsto p 0))

(defn path-ends-at-end [p mz]
  (let [last-cell (dec (* (first mz) (first mz)))]
    (fresh [h t]
           (firsto p h)
           (resto p t)
           (conde
            ((== h last-cell)
             (emptyo t))
            ((path-ends-at-end t mz))))))

(defn valid-path [p mz]
  (fresh [h0 h1 t]
         (conde 
          ;; empty path is valid
          ((emptyo p))
          ;; path with one entry is valid
          ((resto p t)
           (emptyo t))
          ;; or everything below
          ((firsto p h0)
           (resto p t)
           (firsto t h1)
           ;; The first two cells are in the maze, in either order
           (path-in-maze [h0 h1] mz)
           ;; The rest of the path is valid.
           (valid-path t mz)))))

(defn no-back-track [p]
  (fresh [t0 t1 h0 h1]
         (conde
          ((emptyo p))
          ((resto p t0)
           (emptyo t0))
          ((resto p t0)
           (resto t0 t1)
           (emptyo t1))
          ((resto p t0)
           (firsto p h0)
           (resto t0 t1)
           (firsto t1 h1)
           (!= h1 h0)
           (no-back-track t0)))))

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
