(ns longest-repeating)

(defn longest-repeating [str]
  (loop [str str
         longest 1]
    (if-not (seq str) longest
            (let [x (first str) xs (rest str) fxs (first xs)]
              (if-not fxs longest
                      (if (= x fxs) (recur xs (inc longest))
                          (let [nextL (longest-repeating xs)]
                            (if(> nextL  longest) nextL longest) )))))))

;; (longest-repeating "ABAB" 2)
(longest-repeating "AAABCCCBDDKK")

(defn replace-first [xs x] (conj (rest xs) x))

(defn longest-repeating-replacement
  ([str repCount] (longest-repeating-replacement str repCount 1))
  ([str repCount longest]
   (loop [str str cnt repCount longest longest]
     (if-not (seq str) longest
             (let [x (first str) xs (rest str) fxs (first xs)]
               (if-not fxs longest
                       (if (= x fxs) (recur xs cnt (inc longest))
                           (let [ thisLongest (if (> cnt 0)
                                                (longest-repeating-replacement (replace-first xs x) (dec cnt) (inc longest))
                                                longest)
                                 nextL (longest-repeating-replacement xs cnt )]
                             (if(> nextL  thisLongest) nextL thisLongest) ))))))))

(longest-repeating-replacement "AABABBA" 1)

(defn is-palindrome [s]
  (if-not  (seq s) true
           (if (= (first s) (last s))
             (recur (butlast (rest s)))
             false)))

(defn  longest-palindrome [s]
  (if-not (seq s) nil
    (let [longest-s (if (is-palindrome s) s (longest-palindrome (butlast s)))
          longest-rest (longest-palindrome (rest s))]
      (apply str (if (> (count longest-rest)  (count longest-s)) longest-rest longest-s )) )))

(longest-palindrome "cbbd")

