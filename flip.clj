(ns flip)
(def k  (fn [n]
          (fn [a b ] (n b a)))) 

; (= 3 ((k nth) 2 [1 2 3 4 5]))



; (= (__ 2 [1 2 3 4 5]) '(3 4 5 1 2))

(def m ( fn [x y] (let [ sp (if (> x 0) x (- (count y) x))    [a b] (split-at sp y)] (println sp) (concat b a)))) 

; (m -2 [1 2 3 4 5]) 
; (range i (quot (count %1) %2) )
; 
; (def x #(for [i  (range 0 %2 )]  (for [j (range i (count %1)  %2)] (nth %1 j))))

(x [1 2 3 4 5 6] 2)
