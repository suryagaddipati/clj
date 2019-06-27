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
                    

; (def x (fn [func]  (fn [ & args]) 
;                    (reduce #(%1 %2) func args)))

; ((x (fn [a]
;         (fn [b]
;           (fn [c]
;             (fn [d]
;               (+ a b c d))))))
;  1 2 3 4)
   

(def x (fn m [ & args] 
         (lazy-seq
           (let [calc ((second args) (first args))
                  restArgs (rest args)
                  fRestArgs (first restArgs)
                  restRestArgs (rest restArgs)
                  rotatedArgs (conj (vec restRestArgs)  fRestArgs)]
              (cons (first args) (apply m calc rotatedArgs))))))  
             

          

; (take 3 ( x 3.14 int double))
(take 5 (x 3 #(- % 3) #(+ 5 %)))

  
