(ns fourClj)
     

(defn rr [xs x]) 
  

(defn desc[ar] (vec (reduce #(if (= %2 (last %1)) 
                              (let [splts (split-at (- (count %1) 2) %1)
                                    fst (first splts)
                                    [cnt elm] (second splts)]
                                (concat fst [(inc cnt)elm]))
          
                              (concat %1 [1 %2])) [] ar)))


         
           

; [[1 1] [2 1] [1 2 1 1]] 
   
; (take 3 (x [[ 1 1]]))
; (take 3 (x [ 1]))

; (def x (fn [func] (fn [ & args] 
;                     (println (func 1)))))
                    

(def x (fn [func]  (fn [ & args] 
                     (reduce #(%1 %2) func args))))

((x (fn [a]
        (fn [b]
          (fn [c]
            (fn [d]
              (+ a b c d))))))
 1 2 3 4)
   

  
