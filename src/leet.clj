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


((fn k [xs n]
   (let [ sum #(reduce + %)
         sets (fn [x n] (take-while #(< (sum %) n) (map #(repeat % x)(range))))]
     (if (or (= n 0) (empty? xs))
             []
             (reduce  #(let [ rest-n (- n (sum %2))
                             rest-k  [] ;(k (rest xs) rest-n)
                             all-sums (map (fn[xs](concat %2 xs) rest-k))] (concat %1 all-sums))
                      []
                      (sets (first xs)))))) [2,3,5] 8)
