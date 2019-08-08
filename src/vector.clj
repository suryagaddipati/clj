(ns vector)

(defn empty?[xs]
  (or (clojure.core/empty? xs)
     (every? #(or (nil? %) (clojure.core/empty? %)) xs)))
(defn max[xs] (apply clojure.core/max  xs))

(defn with-index [xs] (map-indexed #(identity [%1 %2]) xs))


(defn + [xs] (reduce clojure.core/+ xs))
(defn * [xs] (reduce clojure.core/* 1 xs))

(defn except
  ([xs] (except [] xs []))
  ([pxs [x & xs] out]
   (if (nil? x)
     out
     (recur
      (conj pxs x)
      xs
      (conj out (into pxs xs))) )))

