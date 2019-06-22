(ns forClj)
; 
; (j)
(def x (fn [c] (vals (reduce #(let [t (type %2) xs (get %1 t []) ] (assoc %1 t (conj xs %2 ))) {} c))))
; (x [1 :a 2 :b 3 :c])

(def x (fn [x] (first (reduce #(let [st (second %1) ar (first %1)] (if (contains? st %2) [ar st] [(conj ar %2) (conj st %2) ] ))  [[] #{}]  x))))

; (x [1 2 1 3 1 2 4])

(def x (fn [& funs] 
         (fn [& args] 
           (first(loop [fns (reverse funs) result args]
                  (if (nil? (first fns) )result  
                    (recur (rest fns) [ (apply (first fns) result)])))))))
            
; ( (x rest reverse) [1 2 3 4])


(def x #(for [x (range (quot (count %2)  %1))] (for [y (range (* x %1 ) (* x (inc %1)  ))] y)))

(x 3 (range 9))
