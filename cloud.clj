(defn both0 [one two] (and (= one 0) (= two 0)))
(defn lessof [one two] (if (< one two) one two))
(defn jumpingOnClouds [c]
  (defn countJ [cnt idx]  (if (>= idx (dec (count c))) cnt
                                (let [oneIdx (inc idx) twoIdx (+ idx 2)
                                      {one oneIdx two twoIdx} c
                                      nCnt (inc cnt)]
                                  (if (both0 one two)  (lessof (countJ  oneIdx  nCnt) (countJ  twoIdx  nCnt))
                                      (if (= one 0)  (countJ  oneIdx  nCnt) (countJ  twoIdx  nCnt))))))
  (countJ 0 0))
(jumpingOnClouds [0 0 0])
