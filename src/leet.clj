(ns leet
  (:require [clojure.string :as str]))
(import 'java.util.regex.Pattern)

((fn k[x]
   (let [split (fn[x, y]  (map str/trim (clojure.string/split x (Pattern/compile y Pattern/LITERAL))))]
     (cond
       (str/includes? x "+") (reduce #(+ %1 (k %2)) 0 (split x "+"))
           (str/includes? x "*") (reduce #(* %1 (k %2))  1 (split x "*"))
           (str/includes? x "/") (reduce #(quot (k %1) (k %2) )  (split x "/"))
           :else (Integer/parseInt x)))) "3 / 2 *3")

(defn pp[x] (do (print x) x))
(defn sum [xs] (reduce + xs))

(defn repeat-till[x n] (take-while #(<= (sum %) n) (map #(repeat % x)(range))))
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
   )(sort [10,1,2,7,6,1,5])  8)
