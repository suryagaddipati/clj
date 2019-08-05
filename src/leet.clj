(ns leet
  (:require [clojure.string :as str])
  (:require [clojure.spec.alpha :as s]))
(import 'java.util.regex.Pattern)

;util
(defn pp [x] (do (print x) x))
(defn sum [xs] (reduce + xs))

(defn repeat-till [x n] (take-while #(<= (sum %) n) (map #(repeat % x) (range))))

(defn rotate
  ([xs] (conj (vec (rest xs)) (first xs)))
  ([xs n] (reduce (fn [xs x] (conj xs (rotate (last xs)))) [xs] (range 1 n))))

;qs-227


((fn k [x]
   (let [split (fn [x, y]  (map str/trim (clojure.string/split x (Pattern/compile y Pattern/LITERAL))))]
     (cond
       (str/includes? x "+") (reduce #(+ %1 (k %2)) 0 (split x "+"))
       (str/includes? x "*") (reduce #(* %1 (k %2))  1 (split x "*"))
       (str/includes? x "/") (reduce #(quot (k %1) (k %2))  (split x "/"))
       :else (Integer/parseInt x)))) "3 / 2 *3")

;qs-39
((fn ki [xs n]
   (if (or (= n 0) (empty? xs))
     []
     (let [fst (first xs)
           rst (rest xs)
           f-sets (repeat-till fst n)]
       (reduce  #(let [sum-set (sum %2)
                       rest-n (- n sum-set)
                       rest-k  (ki rst rest-n)
                       all-sums (map (fn [xs] (concat %2 xs)) rest-k)]
                   (concat %1  (if (= sum-set n) [%2] all-sums)))
                []  f-sets)))) [2 3 6 7] 7)


;qs-40


((fn ki [xs n]
   (let [all-sets
         (if (or (<= n 0) (empty? xs))
           #{}
           (let [fst (first xs)
                 rst (rest xs)
                 f-sets [#{} #{fst}]]
             (reduce  #(let [sum-set (sum %2)
                             rest-n (- n sum-set)
                             rest-k  (ki rst rest-n)
                             all-sums (map (fn [xs] (concat %2 xs)) rest-k)]
                         (clojure.set/union %1 (if (= sum-set n) #{%2} all-sums)))
                      #{}  f-sets)))] (reduce conj #{} all-sets))) (sort [2,5,2,1,2])  5)

;qs-238
((fn [xs]
   (let [left-sum (fn [xs] (reduce (fn [xs x]
                                     (let [lst  (if (empty? xs) [1 1] (last xs))]
                                       (conj xs [(* (first lst) (last lst)) x]))) [] xs))
         left (left-sum xs)
         right (reverse (left-sum (reverse xs)))]
     (map #(* (first %1) (first %2)) left right))) [1 2 3 4])

;qs-46
((fn k [xs]
   (if (= 1 (count xs)) [xs]
       (let [rots (rotate xs (count xs))]
         (reduce (fn [xs x]
                   (let [fst (first x)
                         rst (rest x)
                         rst-combs (k rst)
                         fcombs (map #(conj % fst) rst-combs)]
                     (concat xs fcombs)))  [] rots)))) [1,1,2])

;qs-64
((fn k
   ([xs] (k xs 0))
   ([xs n]
    (let [fst (first (first xs))
          left   (map rest xs)
          bottom  (rest xs)
          dleft #(k left (+ n fst))
          dbottom #(k bottom (+ n fst))]
      (cond
        (nil? fst) n
        (every? empty? left) (dbottom)
        (every? empty? bottom) (dleft)
        :else (if (< (dbottom) (dleft)) (dbottom) (dleft))))))
 [[1,3,1],
  [1,5,1],
  [4,2,1]])

(defn matrix-size [matrix]
  [(count matrix)
   (count (first matrix))])

(defn neighbors [matrix pair visited]
  (let [[m n] (matrix-size matrix)
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

(s/def ::matrix-pair
  (fn [matrix-pair]
    (let [[matrix pair] matrix-pair
          [m n]  (matrix-size matrix)
          [x y]  pair] (and (< x m) (< y n)))))

(s/check-asserts true)

(defn at-pos [matrix pair]
  (s/assert ::matrix-pair [matrix  pair])
  (nth (nth matrix (first pair)) (second pair)))

;qs-79
((fn k
   ([xs x]
    (k xs x [[0 0]] #{}))
   ([xs x pxs visited]
    (let [next-neighbors (reduce  (fn [[js visited] j]
                                    (if (= (first x) (at-pos xs j))
                                      [(concat js (neighbors xs j visited)) (conj visited j)]
                                      [js visited]))
                                  [[] visited]
                                  pxs)]
      (cond
        (empty? x) true
        (empty? (first next-neighbors)) false
        :else  (k xs (rest x) (first next-neighbors) (second next-neighbors))))))
 [[\A \B \C \E]
  [\S \F \C \S]
  [\A \D \E \E]]
 "ABFDEX")
