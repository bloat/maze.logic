(ns maze.logic.attempt1
  (:refer-clojure :exclude [==]) 
  (:use [clojure.core logic]))

(def a-maze
  [10 #{[35 34] [69 68] [6 5] [7 6] [8 7] [9 8] [41 40] [42 41] 
        [45 44] [46 45] [16 15] [49 48] [19 18] [83 82] [52 51]
        [84 83] [85 84] [54 53] [86 85] [55 54] [26 25] [27 26]
        [59 58] [91 90] [28 27] [92 91] [93 92] [94 93] [31 30]
        [63 62] [96 95] [1 0] [65 64] [2 3] [98 99] [10 11] [42 43]
        [11 12] [75 76] [77 78] [78 79] [20 21] [21 22] [22 23]
        [88 89] [96 97] [1 2] [97 98] [3 13] [84 94] [86 96] [67 57]
        [69 59] [38 28] [70 60] [39 29] [71 61] [41 31] [73 63]
        [42 32] [11 1] [43 33] [75 65] [76 66] [45 35] [77 67] [14 4]
        [46 36] [47 37] [79 69] [48 38] [80 70] [49 39] [81 71]
        [82 72] [19 9] [83 73] [20 10] [52 42] [84 74] [85 75] [87 77]
        [24 14] [56 46] [57 47] [26 16] [90 80] [27 17] [59 49]
        [91 81] [60 50] [29 19] [30 20] [62 52] [65 55] [97 87]
        [34 24] [66 56] [98 88]}])

(def a-maze2 [2 #{[2 3] [3 1] [2 0]}])
(def a-maze3 [3 #{[4 3] [1 0] [1 2] [2 5] [3 6] [4 7] [5 8] [1 4]}])
(def a-maze4 [4 #{[2 1] [5 6] [6 7] [10 11] [12 13] [13 14] [14 15] [4 0] [6 2] [7 3] [8 4] [9 5] [12 8] [13 9] [14 10]}])

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

(mz-solve a-maze)
