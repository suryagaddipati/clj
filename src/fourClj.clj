     

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
; (take 5 (x 3 #(- % 3) #(+ 5 %)))

(def x (fn k [xs] (map  #(loop [out %] 
                          (if-not (sequential? (first out)) 
                           out 
                           (recur (first out)))) xs))) 
;; (defn pp [x] (do (println x) x))                  

(def x (fn [xs] 
          (letfn [(flat [x]
                        (loop [out x] 
                          ( if-not (sequential? (first out))   
                            out
                           (recur (first out)))))] 
            (reduce #(conj  %1   (flat %2)   ) []  xs))))


;; (x '((1 2) ((3 4) ((((5 6)))))))

(def x (fn [& sequs]
         (loop [sqs sequs]
           (if (every? #(= (first %) (first (first sqs))) sqs)
             (first(first sqs))
             (let [sorted-sqs (sort-by first sqs)
                   smallest (first (first sorted-sqs))
                   [all-small-sqs all-big-sqs] (partition-by #(= (first %) smallest) sorted-sqs) ]
               (recur (concat all-big-sqs (map rest all-small-sqs) ))
               )))))

;; (x [3 4 5])
;; (x [1 2 3 4 5 6 7] [0.5 3/2 4 19])

(def x (fn [n-in pred arr]
         (loop [n n-in
                xs arr
                out []]
           (if (and (= n 1) (pred (first xs)) ) out 
               (let [f (first xs)
                     fs (rest xs)
                     is-match (pred f)
                     new-N (if is-match (dec n) n)]
                 (recur new-N fs (conj out f)))))))

(x 4 #(= 2 (mod % 3))
    [2 3 5 7 11 13 17 19 23])
(x 3 #(some #{\i} %)
    ["this" "is" "a" "sentence" "i" "wrote"])

;; (def x (fn [pred ins xs]
;;          (let [pairs (partition 2 1 [] xs)]
;;            (reduce #(if (< (count %2) 2) (concat %1 %2)
;;                         (if (apply pred %2)
;;                           (conj %1 (first %2) ins  )
;;                           (conj %1 (first %2)))) [] pairs))))
(def x (fn [pred ins xs]
         (let [pairs (partition 2 1 [] xs)]
           (mapcat #(if (=(count % ) 1) %
                        (let [f (first %)
                              s (second %)]
                          (if (pred f s) [f ins ] [f ])) )
                   pairs)
           )))
(x < :less [1 6 7 4 3])


;; (->> [0 1]
;;      (iterate (fn [[a b]] [b (+ a b)]))
;;      (map first))
