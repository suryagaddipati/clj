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
