(require '[clojure.core.reducers :as r] '[clojure.string :as s])

(defn count-occurrences [words]
  (r/fold
   (r/monoid #(merge-with + %1 %2) (constantly {}))
   (fn [m [k cnt]] (assoc m k (+ cnt (get m k 0))))
   (r/map #(vector % 1) words)))

(defn words [s] (s/split s #"\s+"))

(defn wordCount [s]
  (count-occurrences s))

(defn checkMagazine  [magazine note]
  (loop [noteWords  note
         mag (wordCount magazine)]
    (let [x (first noteWords)
          xs (rest noteWords)
          magCount (get mag x)]
      ; (println x xs magCount)
      (if-not x "Yes"
              (if (or (nil? magCount) (= magCount 0))    "No"
                  (recur xs (assoc mag x (dec magCount))))))))

(checkMagazine ["give" "me" "one"  "grand" "today" "night"]  ["give" "one" "grand" "today"])
