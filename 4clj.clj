
(def x  (fn  [& fns]  (fn  [& args]  (let [k (for  [f fns]  (apply f args))] (vec k)))))

; (((x + max min) 2 3 5 1 6 4))
(def k ((x + max min) 2 3 5 1 6 4))
; (= [21 6 1] k)

(def k (fn [s] (sort #(compare (clojure.string/lower-case %1) (clojure.string/lower-case %2))  (re-seq #"\w+" s))))
; (def k (fn [s] (sort #(compare %1 %2)  (re-seq #"\w+" s))))
; (k "Have a nice day.")

; (defn ana [xs] ())
;k (ana ["meat"  "mat"  "team"  "mate"  "eat"])

(def x (fn [n b]
         (loop [result []
                number n]
           (if (= (quot number b) 0) (into [(rem number b)] result)
               (recur (into [(rem number b)] result) (quot number b))))))

(x 1234501 10)
