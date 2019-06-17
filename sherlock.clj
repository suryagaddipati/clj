(require '[clojure.core.reducers :as r])

(defn anagrams [win]
  (loop [x first
         xs rest
         anas []]))

(defn windows [col]
  (for [s (range 1 (quot (count col) 2))]
    (loop [wins []
           cols col]
      (if (< (count cols) s) wins
          (recur (conj wins (subvec cols 0 s)) (subvec cols 1))))))

(defn sherlockAndAnagrams  [s]
  (for [win (windows (vec s))] (anagrams win)))

(sherlockAndAnagrams "ifailuhkqq")
