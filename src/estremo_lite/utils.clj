(ns estremo-lite.utils
  (:use clojure.math.numeric-tower))

(def *base-map* {0 \A 1 \C 2 \G 3 \T})

(defn sum
  "Return the sum of xs"
  [xs]
  (reduce (fn [x y] (+ x y)) 0 xs))

(defn slice
  "Python-style slices"
  [xs i j]
  (->> xs
       (drop i)
       (take (- j i))))

(defn random-site
  "Return random nucleotide sequence of length n"
  [n]
  (map *base-map*
       (repeatedly n (fn [] (rand-int 4)))))

(defn my-count [xs]
  (reduce (fn [x y] (+ x 1)) 0 xs))

(defn sigmoid [x]
  (/ 1 (+ (expt Math/E (- x)) 1)))