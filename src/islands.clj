(ns islands)

(defn islands [s]
  (let [x (count (first s))
        y (count s)
        gt (fn [i] (let [xi (rem i x) yi (quot i x) ] (if (>= i 0 )(nth (nth  s yi) xi)  0)))
        startOfIsland #(let [topIdx (- % x) leftIdx (dec %1)]
                         (and (= (gt topIdx) 0) (= (gt leftIdx) 0)))
        ]
    (reduce #( if (= (gt %2) 0) %1
              (if (startOfIsland %2) (inc %1) %1))  0 (range (* x y)) )))

  ;; (islands [
  ;;            [ 1 1 0 0 0]
  ;;            [ 1 1 0 0 0]
  ;;            [ 0 0 1 0 0]
  ;;            [ 0 0 0 0 0]
  ;;           ])

(islands

          [[1,1,1],[0,1,0],[1,1,1]]
          )
