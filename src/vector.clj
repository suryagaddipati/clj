(ns vector)

(defn empty?[xs]
  (or (clojure.core/empty? xs)
     (every? #(or (nil? %) (clojure.core/empty? %)) xs)))
(defn max[xs] (apply clojure.core/max  xs))
(defn min[xs] (apply clojure.core/min  xs))

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

(defn contigous-windows[xs]
  (if (empty? xs) []
      (let [windows (loop [n  1
                           xs xs
                           out [[(first xs)]]]
                      (if (=  n (count xs)) out
                          (let [[y & ys] xs
                                win (first (split-at n ys))
                                y+win (concat [y] win)
                                ] (recur (inc n) xs (conj out y+win))

                               )))
            nxt-windows (contigous-windows (rest xs)) ]
        (concat windows nxt-windows) )))

