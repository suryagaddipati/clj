(ns matrix
  (:require [clojure.spec.alpha :as s])
  (:require [vector :as v]))

(defn size [matrix]
  "[rows * cols]"
  [(count matrix)
   (count (first matrix))])

(defn neighbors [matrix pair visited]
  "[[top][left][right][bottom]]"
  (let [[m n] (size matrix)
        [x y] pair
        x+ (inc x)
        x- (dec x)
        y+ (inc y)
        y- (dec y)]
    (remove #(contains? visited %)
            (reduce (fn [ks k]
                      (cond
                        (= k 0) (if (>= x- 0) (conj ks [x- y]) ks)
                        (= k 1) (if (>= y- 0) (conj ks [x y-]) ks)
                        (= k 2) (if (< x+ m) (conj ks [x+ y]) ks)
                        (= k 3) (if (< y+ n) (conj ks [x y+]) ks))) [] (range 4)))))

(defn at-pos [matrix pair]
  "element at position [m n]. M rows, N cols."
  (s/assert ::matrix-pair [matrix  pair])
  (nth (nth matrix (first pair)) (second pair)))

(s/check-asserts true)
(s/def ::matrix-pair
  (fn [matrix-pair]
    (let [[matrix pair] matrix-pair
          [m n]  (size matrix)
          [x y]  pair] (and (< x m) (< y n)))))

(defn rows [xs] xs)

(defn cols [xs]
  (if (v/empty? xs)
    nil
    (into [(map first xs)] (cols (map rest xs)))))

(defn with-index [xs]
  "([[m-idx n-idx] el] ...)"
  (reduce  (fn [xs x]
             (let [[idx-x ys] x]
               (concat xs (map #(let [[idx-y el] %] [[idx-x idx-y] el])  (v/with-index ys)))))
           []
           (v/with-index xs)))