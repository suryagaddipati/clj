
(defn rotLefti  [a d]
  (if (= d 0) a (recur (let [x (first a)
                             xs (rest a)
                             rot (conj (vec xs) x)]
                         rot)  (dec d))))
(defn rotLeft  [a d] (let [len (count a)
                           rm (mod d len)
                           qou (quot d len)] (rotLefti a rm)))

(rotLeft [1 2 3 4 5] 4)
