     

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
                           (conj xs (reduce (partial merge x) #{} prevSet)))
                         #{} xs))))))

;; (x 2 #{0 1 2})


(def x (fn k
         ([n] (k n 0))
         ([n cnt]
          (let [toInt #(Integer/parseInt (str %))
                sumSqs (reduce + (map #(* % %)  (map toInt (seq (str n)))))]
            (if (= Integer/MAX_VALUE cnt) false
                (if (= 1 sumSqs) true (recur sumSqs (inc cnt))))))))

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

(def x (fn [& sets]
         (letfn [(sums
                   ([xs] (sums xs []))
                   ([xs out]
                    (if (empty? xs) out
                        (let [x (first xs)
                              prevSums (sums (rest xs) out)
                              xSums (map #(+ x %) prevSums)]
                          (into (conj xSums x) prevSums)))))]
           (let [alSums (map #(set (sums %)) sets)]
             (not (empty? (reduce  clojure.set/intersection  alSums)))))))

;; (x #{-1 1 99} 
;;     #{-2 2 888}
;;     #{-3 3 7777})

(def x (fn k [n]
         (if (= n 0) #{""}
             (reduce #(conj %1  (str "()" %2)
                            (str "(" %2 ")")
                            (str %2 "()")) #{}  (k (dec n))))))

;; (count)


;; (x 4)

;; (def x )
(def x  (fn k [x]
          (let [x   (map #(Integer/parseInt (str %)) (vec (str x)))]
            (case (count x)
              1 x
              2 (let [[f l] x] (if (= f l) x (if (> f l) [f f] [l l])))
              (let [f (first x)
                    l (last x)
                    rest (rest (butlast x))
                    jj (k (str f l))]
                jj)))))


;; (take 16 (__ 162)
;; (x 162)


(def x (fn [fun & args]
         (loop [ret  (apply fun args)]
           (if (fn? ret) (recur (ret))  ret))))

;; (letfn [(triple [x] #(sub-two (* 3 x)))
;;         (sub-two [x] #(stop?(- x 2)))
;;         (stop? [x] (if (> x 50) x #(triple x)))]
;;   (x triple 2))

(def x (fn k
         ([fun x y s t]  (take s (map #(take t %) (k fun x y))))
         ([fun] (k fun 0 0))
         ([fun x y]
          (lazy-seq
           (cons
            (let [k2 (fn k3 [fun x y] (lazy-seq
                                       (cons
                                        (fun x y)
                                        (k3 fun x (inc y)))))]
              (k2 fun x y))
            (k fun (inc x) y))))))


;; (take 5 (map #(take 7 %) (x * 3 5)))

;; (take 5 (x str))

;; (x * 3 5 5 7)

;; (k [n]
;;    (if (= n 2) [2 3]
;;        (let [prev (k (dec n))
;;              n (loop [nxt (+ (last prev) 2)]
;;                  (if (every? #(not= 0 (rem  nxt %)) prev) nxt (recur (+ nxt 2))))]
;;          (conj prev n))))


(def x (fn [n]
         (letfn [(ap [n fun] (let [isPrime (memoize esp)]
                               (loop [n (fun n 2)]
                                 (if (isPrime n) n (recur (fun n 2))))))

                 (primesLessThan [n] (let [isPrime (memoize esp)] (filter isPrime (range 2 n))))

                 ;; (primesLessThan [n] (memoize (plth n)))

                 (esp [n] (cond
                            (= n 2) true
                            (= n 3) true
                            (even? n) false
                            :else  (every? #(not= 0 (rem  n %)) (primesLessThan n))))
                 (avg [x y] (/ (+ x y) 2))]
           (= n (avg (ap n +) (ap n -))))))

(map x [5 53 54 55])


;; (x 5)

;; (def x (fn [n]
;;          (if (= n 1) 1
;;              (let [gcd (fn [a b] (cond (= a b) a
;;                                        (= a 0) b
;;                                        (= b 0) a
;;                                        (> a b) (recur (- a b) b)
;;                                        :else (recur  b (-  b a))))]
;;                (reduce #(if (= (gcd %2 n) 1) (inc %1) %1) 0 (range 1 n))))))

;; (x 40)

;; loop [first-h  nil
;;       last-h   %1
;;       found false]
;; (if found  (conj first-h last-h)
;;     (if (empty? last-h)
;;       (conj first-h [%2 %2])
;;       (let [f-last-h (first last-h)
;;             xs-last-h (rest last-h)
;;             int-start (first f-last-h)
;;             int-end (second f-last-h)
;;             exp (if (or (%2)))  
;;             ]
;;         (recur (conj (vec first-h) f-last-h) xs-last-h false))))


((fn [xs]
   (reduce (fn [xs x]
             (let [lst (last xs)]
               (if (nil? lst) (conj xs [x x])
                   (let [lst-lst (last lst)]
                     (cond
                       (= (inc lst-lst) x) (conj (vec (butlast xs)) [(first lst) x])
                       (= lst-lst x) xs
                       :else (conj (vec  xs) [x x])))))) [] (sort xs))) [1 1 1 1 1 1 1])



;; (take 2 ((fn m[y]
;;            (let [ x (map #(Integer/parseInt (str %)) (str y))
;;                  cnt (count x)
;;                  mid-cnt (quot cnt 2)
;;                  [s1 s2] (split-at mid-cnt x)
;;                   s3 (split-at 1 s2)
;;                  mid (nth x mid-cnt )
;;                  nxtMid  (inc (Integer/parseInt (str mid)))
;;                  nxt  nxtMid]
;;              (lazy-seq (cons y (m nxt))))) 171))

;; 150


(take 10
      ((fn [n]
         (let [
               y (str n)
               fst (if (< n 100) 0 (Integer/parseInt (clojure.string/join (repeat  (dec (count y)) \9))))
               pali (fn m [x]
                      (let [
                            cnt (count y)
                            half (quot cnt 2)
                            inc-val (cond
                                      (every? #(= % \9) y) 2
                                      (= \9 (last (butlast y))) 11
                                      (even? cnt) (*  11 (int (Math/pow 10 (dec half))))
                                      :else  (int (Math/pow 10 half)))

                            nxt  (+ x inc-val)]
                        (lazy-seq (cons x (m nxt)))))
               lst  (last (take-while #(<  % n) (pali 0)))
               lst   (if (nil? lst)  0 lst)
               lst-pali (pali lst)
               ]
          ;; (if (= lst n) lst-pali (rest lst-pali))
           (pali (* 111111111 111111111) )
          )) (* 11 11)))

(take-while #(< % 50) ((fn m [x]
                         (let [y (str x)
                               cnt (count y)
                               half (quot cnt 2)
                               inc-val (cond
                                         (every? #(= % \9) y) 2
                                         (even? cnt) (*  11 (int (Math/pow 10 (dec half))))
                                         :else  (int (Math/pow 10 half)))

                               nxt  (+ x inc-val)]
                           (lazy-seq (cons x (m nxt))))) 0))

((fn [n]
   (let [lst3 (take-last 3
                         (first
                          (take 1
                                (drop-while #(<= (last %) n)
                                            ((fn k [prev]
                                               (let [n (last prev)
                                                     nxt (if (= n 2) 3
                                                             (loop [nxt (+ n 2)]
                                                               (if (every? #(not= 0 (rem  nxt %)) prev)
                                                                 nxt
                                                                 (recur (+ nxt 2)))))]
                                                 (lazy-seq (cons prev (k (conj prev nxt))))))  [2])))))]
     (if (= (second lst3) n) (= (second lst3) (/ (+ (first lst3) (last lst3)) 2))  false))) 5)

((fn [xs]
   (reduce (fn [xs s] (assoc xs (first s) (rest s))) {}
           (reduce #(if (keyword %2)
                      (conj (vec %1) [%2]) (let [lst (last %1)
                                                 bt-lst (butlast %1)]
                                             (conj (vec bt-lst) (conj lst %2)))) [] xs)))
 [:a 1 2 3 :b :c 4])

((fn k [xs]
   (reduce #(if (sequential? (first %2))
              (into %1 (k %2))
              (conj %1 %2)) [] xs))  [[[[:a :b]]] [[:c :d]] [:e :f]])

((fn k [n]
   (let [rep (fn [s n] (clojure.string/join (repeat n s)))]
     (loop [n n
            out ""]
       (cond

         (>  (quot n 1000) 0) (recur (rem n 1000) (str out (rep "M" (quot n 1000))))
         (>  (quot n 100) 0) (recur (rem n 100) (str out (case (quot n 100)
                                                           1 "C"
                                                           2  "CC"
                                                           3 "CCC"
                                                           4 "CD"
                                                           5 "D"
                                                           6 "DC"
                                                           7 "DCC"
                                                           8 "DCCC"
                                                           9 "CM")))
         (>  (quot n 10) 0) (recur (rem n 10) (str out (case (quot n 10)
                                                         1 "X"
                                                         2  "XX"
                                                         3 "XXX"
                                                        4 "XL"
                                                         5 "L"
                                                         6 "LX"
                                                         7 "LXX"
                                                         8 "LXXX"
                                                         9 "XC")))
         (< n 10) (str out (case n
                             0 ""
                             1 "I"
                             2 "II"
                             3 "III"
                             4 "IV"
                             5 "V"
                             6 "VI"
                             7 "VII"
                             8 "VIII"
                             9 "IX")))))) 3999)

((fn k [n xs]
   (if (= n 1)  (reduce #(conj %1 #{%2}) #{} xs)
       (let [prv  (k (dec n) xs)
             into-prv (fn [r] (reduce (fn [xs x] (conj xs (conj x r))) #{} prv))]
         (set
          (filter #(= (count %) n)
                  (reduce (fn [xs x] (into xs (into-prv x))) #{} xs))))))  10 #{0 1 2})

((fn [n x y]
   (letfn [(nums [a]  (take-while #(< % n) (map #(* % a) (range))))]
     (reduce + 0 (concat (nums x) (nums y))))) 10 3 5)

((fn [n x y]
   (letfn [(sums [a]
             (let [q (quot (dec n) a)
                   qt-sum (quot  (*' q (inc q)) 2)]
               (*' qt-sum a)))] (-' (+' (sums x) (sums y)) (sums (*' x y))))) 1000 3 5)

((fn [x]
   (let [ini-count (count x)
         x (conj (conj (conj x {:a :b}) {:c :d}) {:c :d})]
     (cond
       (not (nil? (:a x))) :map
       (= (count x) (+ ini-count 2)) :set
       (= (first  x) {:c :d}) :list
       :else :vector))) [1 2 3 4 5 6])

(nth 
      ( (fn [n]
             (let [to-int #(Long/parseLong %)
                   pali  (fn k[x fst]
                           (cond
                             (= x 1) (range 10)
                             (= x 2) (let [one (k 1 false)] (map #(to-int (str %1 %2)) one one))
                             :else (let [prv (k (- x 2) false)](reduce  (fn [ys y]
                                                                          (into ys  (map #(to-int (str y % y )) prv))) []  (if first  (rest (k 1 true)) (k 1 false)) ))
                             ))
                   palis (fn t[x](lazy-seq (concat   (filter #(<= n %)(pali x true)) (t (inc x))  )) )]
               (palis (count (str n))))) 0 ) 10101)
