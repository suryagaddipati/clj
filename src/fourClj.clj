     

(defn rr [xs x])

(defn desc [ar] (vec (reduce #(if (= %2 (last %1))
                                (let [splts (split-at (- (count %1) 2) %1)
                                      fst (first splts)
                                      [cnt elm] (second splts)]
                                  (concat fst [(inc cnt) elm]))

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


(def x (fn m [& args]
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
                     (if-not (sequential? (first out))
                       out
                       (recur (first out)))))]
           (reduce #(conj  %1   (flat %2)) []  xs))))


;; (x '((1 2) ((3 4) ((((5 6)))))))


(def x (fn [& sequs]
         (loop [sqs sequs]
           (if (every? #(= (first %) (first (first sqs))) sqs)
             (first (first sqs))
             (let [sorted-sqs (sort-by first sqs)
                   smallest (first (first sorted-sqs))
                   [all-small-sqs all-big-sqs] (partition-by #(= (first %) smallest) sorted-sqs)]
               (recur (concat all-big-sqs (map rest all-small-sqs))))))))

;; (x [3 4 5])
;; (x [1 2 3 4 5 6 7] [0.5 3/2 4 19])


(def x (fn [n-in pred arr]
         (loop [n n-in
                xs arr
                out []]
           (if (and (= n 1) (pred (first xs))) out
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
           (mapcat #(if (= (count %) 1) %
                        (let [f (first %)
                              s (second %)]
                          (if (pred f s) [f ins] [f])))
                   pairs))))
;; (x < :less [1 6 7 4 3])

;; (defn windows [col]
;;   (for [s (range 1 (count col))]
;;     (loop [wins []
;;            cols col]
;;       (if (< (count cols) s) wins
;;           (recur (conj wins (subvec cols 0 s)) (subvec cols 1))))))

;; (windows [1 :a])
;; (vec [1 2])

;; (def x (fn [s] ))
;; (x #{1 :a})

;; (->> [0 1]
;;      (iterate (fn [[a b]] [b (+ a b)]))
;;      (map first))

;; (def x (fn [xs]
;;          (let [ini [[#{}] (reduce #(conj %1 #{%2})  [] xs )]]
;;            (take (count xs)
;;                  (iterate
;;                   #(let [l (last %)]  l) ini)))))
(conj nil 1)
(conj [[1]] [2])

(for [k []] k)
(conj '() [1])

(def x (fn [xs]
         (set (reduce #(concat %1 (for [y  %1] (conj y %2)))
                      #{#{}} xs))))
;; (x [1 2 3])
;; (x #{1 :a})
(seq [])
;; (is-anagram [x y]
;;   (let [xmap (reduce #(assoc %1 %2  (inc (get %1 %2 0))) {} x)
;;         ymap (reduce #(let [val (get %1 %2 0)]
;;                         (if (= val 1)
;;                           (dissoc %1 %2)
;;                           (assoc %1 %2 (dec val) )))
;;                      xmap  y) ]
;;     (empty? ymap)))
(def x (fn [in]
         (letfn [(is-anagram [x y]
                   (let [xmap (reduce #(assoc %1 %2  (inc (get %1 %2 0))) {} x)
                         ymap (reduce #(let [val (get %1 %2 0)]
                                         (if (= val 1)
                                           (dissoc %1 %2)
                                           (assoc %1 %2 (dec val))))
                                      xmap  y)]
                     (empty? ymap)))]
           (loop [xs in anas #{}]
             (if-not (seq xs) anas
                     (let [y (first xs)
                           ys (rest xs)
                           {yAnas true noAnas false}  (group-by #(is-anagram y %) xs)
                           nAnas (if (> (count yAnas) 1) (conj anas (set yAnas)) anas)]
                       #break
                        (recur noAnas nAnas)))))))

;; (x ["meat"  "mat" "team" "mate" "eat"])
;; (println "dasf")

;; (is-anagram "meat" "mate")

(def x (fn [in] (let [xs (map #(Integer/parseInt %) (clojure.string/split in #","))
                      filtered (filter #(integer? (rationalize (Math/sqrt %))) xs)] (clojure.string/join "," filtered))))
;; (x "4,5,6,7,8,9")

(def x (fn kn [exp]
         (fn [m]
           (let [k (fn [x args]   (case x
                                    / (apply / args)
                                    + (apply + args)
                                    - (apply - args)
                                    * (apply * args)))

                 lookup-arg (fn [arg] (if (seq? arg) ((kn arg) m)  (if (symbol? arg) (get m arg) arg)))
                 fun (first exp)] (k fun (map lookup-arg (rest exp)))))))


;; (map (x '(* (+ 2 a)
;;              (- 10 b)))
;;      '[{a 1 b 8}
;;        {b 5 a -2}
;;        {a 2 b 11}])

;; ((x '(* (+ 2 a)
;;             (- 10 b)))
;;      '{a 1 b 8})


(def x (fn [x]
         (let [xs (seq (str x))
               c (count xs)
               toInt #(Integer/parseInt (str %))
               sum #(reduce + %)
               halfC (quot c 2)
               half1 (map toInt (take halfC xs))
               half2  (map toInt (drop (if (even? c) halfC (inc halfC)) xs))]
           (= (sum half1) (sum  half2)))))


;; (x 89098)


(def x (fn [x] (let [[x & xs] (clojure.string/split x #"-")]
                 (apply str  (into [x] (map clojure.string/capitalize xs))))))

  ;; (x "multi-word-key")


(def x  (fn [x]
          (= x
             (reduce #(if (= (rem x %2) 0) (+ %2 %1) %1) 0 (range 1 (inc (quot x 2)))))))

;; (x 496)

(def x (fn m
         ([fun  xs]
          (m fun (first xs) (rest xs)))
         ([fun x xs]
          (m fun x xs true))
         ([fun x xs isFirst]
          (if (empty? xs) nil
              (lazy-seq
               (let [fnr (fun x (first xs))
                     x  (if isFirst x fnr)
                     xs (if isFirst xs (rest xs))]
                 (cons x (m fun  x xs false))))))))


;; (reduce #(conj %1 (fun (last %1) %2))  [x]  xs)


;; (x conj [1] [2 3 4])
;; (take 5(x + (range)))


(def x (fn [fun & maps]
         (letfn [(app [a b] (if (and a b) (fun a b) (or a b)))
                 (merge  [a b]
                   (let [keys (into (keys a) (keys b))]
                     (reduce #(assoc %1 %2 (app (get a %2) (get b %2))) {} keys)))]
           (reduce merge  maps))))

;; (x * {:a 2, :b 3, :c 4} {:a 2} {:b 2} {:c 5})


(def x (fn k [n xs]
         (letfn [(merge [v xs x] (into xs (conj x v)))]
           (if (= n 0) #{nil}
               (let [prevSet (k (dec n) xs)]
                 (reduce (fn [xs x]
                           (conj xs (reduce (partial merge x)#{} prevSet)))
                         #{} xs))))))

;; (x 2 #{0 1 2})



(def x (fn k
         ([n ](k n 0))
         ([n cnt]
          (let [ toInt #(Integer/parseInt (str %))
                sumSqs (reduce + (map #(* % %)  ( map toInt (seq (str n))))) ]
            (if (= Integer/MAX_VALUE cnt) false
                (if(= 1 sumSqs) true (recur sumSqs (inc cnt))))))))

;; (x 986543210)

(def x (fn [st]
         (empty?
          (let [ps {\( \) \[ \] \{ \}}
                brackets (into (keys ps) (vals ps))
                st (filter #(contains? (set brackets) %) st)]
            (reduce  #(if (= %2  (get ps  (last %1)))
                        (vec (butlast %1))
                        (conj %1 %2))
                     [] st)))))

;; (x "([]([(()){()}(()(()))(([[]]({}()))())]((((()()))))))")
;; (x "[ { {   } []")
;; (x " [This string has no brackets.")

(def x (fn [ & sets]
         (letfn [(sums
                   ([xs] (sums xs []))
                   ([xs out]
                    (if (empty? xs) out
                        (let [ x (first xs)
                              prevSums (sums (rest xs) out)
                              xSums (map #(+ x %) prevSums) ]
                          (into (conj xSums x) prevSums)))))]
           (let [alSums (map #(set (sums %)) sets)]
             (not (empty?(reduce  clojure.set/intersection  alSums)))))))

(x #{-1 1 99} 
    #{-2 2 888}
    #{-3 3 7777})
