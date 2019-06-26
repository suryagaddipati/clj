(defn repeatS [s n] (let [c (count s)
                          cIdx (dec c)
                          minIdx (min (- n c) c)]
                      (if (= c n) s
                          (recur (str s (subs s 0 minIdx)) n))))
(defn countA [cnt s] (if-not (seq s) cnt (let [x (subs s 0 1)
                                               xs (subs s 1 (count s))] (if (= x "a") (recur (inc cnt) xs) (recur cnt xs)))))
(defn count2 [cnt idx n s] (if (>= idx n) cnt
                               (let [len (count s)
                                     nIdx (+ idx len)
                                     aCount (countA 0 s)
                                     nCount (+ cnt aCount)]
                                 (recur nCount nIdx n s))))

(defn repeatedString [s n]
  (let [len (count s)
        aCount (countA 0 s)
        rm (mod n len)
        qou (quot n len)
        f  (- n rm)]
    (+ (* qou aCount)
       (count2 0 0 rm (subs s 0 rm)))))

(repeatedString "aba" 10)
