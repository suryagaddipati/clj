(ns vector)

(defn empty? [xs]
  (or (clojure.core/empty? xs)
      (every? #(or (nil? %) (clojure.core/empty? %)) xs)))
(defn max [xs] (apply clojure.core/max  xs))
(defn min [xs] (apply clojure.core/min  xs))

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
      (conj out (into pxs xs))))))

(defn windows
  ([xs] (windows xs 1 (count xs)))
  ([xs start end]
   (for [s (range start (inc  end))]
     (loop [wins []
            xs xs]
       (if (< (count xs) s) wins
           (recur (conj wins (subvec xs 0 s)) (subvec xs 1)))))))
