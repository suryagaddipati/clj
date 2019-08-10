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

(defn row [xs n](nth (rows xs) n))

(defn cols [xs]
  (if (v/empty? xs)
    nil
    (into [(map first xs)] (cols (map rest xs)))))

(defn col [xs n] (vec (nth (cols xs) n)))

(defn with-index [xs]
  "([[m-idx n-idx] el] ...)"
  (reduce  (fn [xs x]
             (let [[idx-x ys] x]
               (concat xs (map #(let [[idx-y el] %] [[idx-x idx-y] el])  (v/with-index ys)))))
           []
           (v/with-index xs)))

;; (for [mi (range start-m-idx end-m-idx)]
;;   (for [ni (range start-n-idx end-n-idx)](at-pos xs  [mi ni])))

(defn sub-matrix [xs  start sub-m sub-n fn initial-val]
  (if (or (= -1 sub-m) (= -1 sub-n)) [nil nil]
      (let [[m n] (size xs)
            [start-m-idx start-n-idx] start
            end-m-idx (+ start-m-idx sub-m)
            end-n-idx (+ start-n-idx sub-n)]
        (if (or (> end-m-idx m) (> end-n-idx n))
          nil
          (let [start-row (row xs start-m-idx)
                start-col (col xs start-n-idx)
                top-row (subvec start-row start-m-idx end-m-idx)
                left-col (subvec start-col start-n-idx end-n-idx)
                reduce-top-row (reduce fn initial-val top-row)
                reduce-left-row (reduce fn initial-val (rest left-col))]
            (reduce fn initial-val [reduce-top-row reduce-left-row])
            )))))
