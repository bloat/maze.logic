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