
(defn rotLefti  [a d] (let [x (split-at d a)] (concat  (second x) (first x))))

(defn rotLeft  [a d] (let [len (count a)
                           rm (mod d len)
                           qou (quot d len)] (rotLefti a rm)))

(rotLeft [1 2 3 4 5] 4)
