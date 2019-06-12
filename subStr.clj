; (require '[clojure.core.reducers :as reducers])

(defn twoStrings  [s1 s2]
  (let [s1Set (set  (seq s1))]
    (loop [s2Seq   (seq s2)]
      (let [x (first s2Seq)
            xs (rest s2Seq)]
        ; (println s1Set)
        (if-not x "NO" (if (contains?  s1Set x) "YES" (recur xs)))))))

(twoStrings "hi" "world")
