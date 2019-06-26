(ns sherlock)
(require '[clojure.core.reducers :as r])

(defn count-occurrences [words]
  (r/fold
   (r/monoid #(merge-with + %1 %2) (constantly {}))
   (fn [m [k cnt]] (assoc m k (+ cnt (get m k 0))))
   (r/map #(vector % 1) words)))

(defn is-anagram  [magazine note]
  (loop [noteWords  note
         mag (count-occurrences magazine)]
    (let [x (first noteWords)
          xs (rest noteWords)
          magCount (get mag x)]
      (if-not x true
              (if (or (nil? magCount) (= magCount 0))   false
                  (recur xs (assoc mag x (dec magCount))))))))

(defn all-anagrams [x xs]
  ; (println x xs)
  (r/fold
   (r/monoid #(if (is-anagram x %2) (conj %1 [x  %2]) %1) (constantly []))
   xs))

(defn anagrams [window]
  (println window)
  (loop [win window
         anas []]
    (let [x (first win)
          xs (rest win)]
      (if (empty? xs) anas
          (recur xs (concat (all-anagrams x xs) anas))))))

(defn windows [col]
  (for [s (range 1 (count col))]
    (loop [wins []
           cols col]
      (if (< (count cols) s) wins
          (recur (conj wins (subvec cols 0 s)) (subvec cols 1))))))

(defn sherlockAndAnagrams  [s]
  (let [anas (for [win (windows (vec s))] (anagrams win))]
    (r/fold (r/monoid #(+ %1 (count %2)) (constantly 0)) anas)))

; (sherlockAndAnagrams "ifailuhkqq")
(sherlockAndAnagrams "abba")
; (sherlockAndAnagrams "ifailuhkqqhucpoltgtyovarjsnrbfpvmupwjjjfiwwhrlkpekxxnebfrwibylcvkfealgonjkzwlyfhhkefuvgndgdnbelgruel")
