(ns map)

(defn assoc-vec[m k v]
  (if (contains? m k)
    (assoc m k  (conj (get m k) v))
    (assoc m k [v])))


(defn graph[xs]
  (reduce (fn [map [k v]]
            (if (contains? map k)
              (let [k-val-map (get map k)
                    v-val-in-k-val-map (get k-val-map v)]( if (nil? v-val-in-k-val-map)
                                                          (assoc map k (assoc k-val-map  v 1 ))
                                                          (assoc map k  (assoc k-val-map v (inc v-val-in-k-val-map))) )  )
              (assoc map k {v 1}))  ) {} xs))


(defn first-get-remove [graph key]
  (let [ key-val (get graph key)
        keys-key-val (keys key-val)
        first-key (first keys-key-val)
        first-key-val (get key-val first-key)]
    (if (empty? key-val)
      [graph nil]
      [(assoc graph key (if (= 0 (dec first-key-val))(dissoc key-val first-key) (assoc key-val first-key (dec first-key-val)) ) ) first-key]
        )))
