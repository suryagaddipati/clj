
(def x  (fn  [& fns]  (fn  [& args]  (let [k (for  [f fns]  (apply f args))] (vec k)))))

; (((x + max min) 2 3 5 1 6 4))
(def k ((x + max min) 2 3 5 1 6 4))
; (= [21 6 1] k)

(def k (fn [s] (sort #(compare (clojure.string/lower-case %1) (clojure.string/lower-case %2))  (re-seq #"\w+" s))))
; (def k (fn [s] (sort #(compare %1 %2)  (re-seq #"\w+" s))))
(k "Have a nice day.")
