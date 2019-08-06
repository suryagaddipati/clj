(ns vector)

(defn empty?[xs]
  (or (clojure.core/empty? xs)
     (every? #(or (nil? %) (clojure.core/empty? %)) xs)))
(defn max[xs] (apply clojure.core/max  xs))

(defn with-index [xs] (map-indexed #(identity [%1 %2]) xs))
