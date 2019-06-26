(0 0 0 0 1 0defn countingValleys [n s]
  (defn countV [prev cnt  st inV] (if-not (seq st) cnt
                                          (let [[f & xs] st]

                                            (condp = f
                                              \U (if inV (recur f (inc cnt) xs false) (recur f cnt xs false))
                                              \D  (recur f cnt xs (= f prev))))))
  (countV \x 0 s false))

(countingValleys 8 "DDUDDUUDUU")
