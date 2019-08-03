(ns leet
  (:require [clojure.string :as str]))
(import 'java.util.regex.Pattern)

;util
(defn pp[x] (do (print x) x))
(defn sum [xs] (reduce + xs))

(defn repeat-till[x n] (take-while #(<= (sum %) n) (map #(repeat % x)(range))))

;227
((fn k[x]
   (let [split (fn[x, y]  (map str/trim (clojure.string/split x (Pattern/compile y Pattern/LITERAL))))]
     (cond
       (str/includes? x "+") (reduce #(+ %1 (k %2)) 0 (split x "+"))
           (str/includes? x "*") (reduce #(* %1 (k %2))  1 (split x "*"))
           (str/includes? x "/") (reduce #(quot (k %1) (k %2) )  (split x "/"))
           :else (Integer/parseInt x)))) "3 / 2 *3")

; 39
((fn ki [xs n]
   (if (or (= n 0) (empty? xs))
     []
     (let [ fst (first xs)
            rst (rest xs)
           f-sets (repeat-till fst n)]
       (reduce  #(let [ sum-set (sum %2)
                       rest-n (- n sum-set)
                       rest-k  (ki rst rest-n)
                       all-sums (map (fn[xs](concat %2 xs)) rest-k)]
                   (concat %1  (if (= sum-set n) [ %2] all-sums)))
                []  f-sets
                )))
   ) [2 3 6 7 ] 7)


;40

((fn ki [xs n]
   (let [ all-sets
         (if (or (<= n 0) (empty? xs))
           #{}
           (let [ fst (first xs)
                 rst (rest xs)
                 f-sets [#{} #{fst}]]
             (reduce  #(let [ sum-set (sum %2)
                             rest-n (- n sum-set)
                             rest-k  (ki rst rest-n)
                             all-sums (map (fn[xs](concat %2 xs)) rest-k)]
                         (clojure.set/union %1 (if (= sum-set n) #{ %2} all-sums) ))
                      #{}  f-sets
                      ))
           )] (reduce conj #{} all-sets))
   )(sort [2,5,2,1,2] )  5)

;238
( (fn [xs]
    (let [left-sum (fn [xs] (reduce (fn [xs x]
                                      (let [lst  (if (empty? xs) [1 1] (last xs))]
                                        (conj xs [(* (first lst) (last lst)) x])
                                        )) [] xs))
          left (left-sum xs)
          right (reverse (left-sum (reverse xs)))]
      (map #(* (first %1)(first %2)) left right))
    ) [1 2 3 4])
;; (rotate (last %1))
(defn rotate
  ([xs] (conj (vec (rest xs)) (first xs)))
  ([xs n] (reduce (fn[xs x](conj xs (rotate (last xs)))) [xs](range 1 n)))
  )

;46
((fn k[xs ]
   (if (= 1 (count xs)) [ xs]
       (let [rots (rotate xs (count xs))]
         (reduce (fn [xs x]
                   (let [fst (first x)
                         rst (rest x)
                         rst-combs (k rst)
                         fcombs (map #(conj % fst) rst-combs)
                         ]
                     (concat xs fcombs)
                     ))  [] rots)))) [1,2,3])


;;
