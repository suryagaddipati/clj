(ns triplets)
(require '[clojure.core.reducers :as r])

(defn inc-map-by [incV m k]
  (if (nil? k) m (let [v (get m k 0)] (assoc m k (+ v incV)))))

(defn inc-v [m k]
  (if (vector? k) (get m (first k) 1)  1))

(defn incr-map [m & ks]
  (r/fold (r/monoid #(inc-map-by (inc-v %1 %2) %1 %2)  (constantly m)) ks))

(defn in-series? [x r]
  (or (= x 1) (= (rem x r) 0)))

(defn doublet [x r]
  (if (= x 1) nil [(quot x r) x]))

(defn prev-doublet [x r]
  (if (= x 1) nil
      (let [prev (quot x r)]
        (if (= prev 1) nil [(quot prev r) prev]))))

(defn foldTriplet [r xs x]
  (if-not (in-series? x r)  xs
          (let [m (first xs)
                sum (second xs)
                dblt (doublet x r)
                pDblt (prev-doublet x r)]
            [(incr-map m x dblt) (+ sum (get m pDblt 0))])))

(defn countTriplets [arr r]
  (second (r/fold (r/monoid (partial foldTriplet r) (constantly [{} 0])) arr)))

(countTriplets [1 3 9 9 27 81] 3)
