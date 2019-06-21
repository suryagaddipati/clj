(ns frequency)
(require '[clojure.core.reducers :as r])


(defn updateCount [m v in]
  ; (println m v)
  (let [
        prvCnt (get m v 0)
        prvCntKey (format "count-%d" prvCnt) 

        newCnt (if in (inc prvCnt) (dec prvCnt))
        newCntKey (format "count-%d" newCnt) 
        numUpdatedM (assoc m v  newCnt)

        removePrevCountM (assoc numUpdatedM prvCntKey (disj (get m prvCntKey #{} )v))
        addNewCountM (assoc removePrevCountM newCntKey (conj (get m newCntKey #{} )v))]
    ; (println removePrevCountM  addNewCountM)
    addNewCountM)) 

(defn updateCounti [m v insert]
  ; (println m v)
 (if (and (not insert) (not (contains? m v))) m (updateCount m v insert)))
  
    
         
    
                         
(defn folder [xs x] 
  (let [cnt (first xs)
        m (second xs)
        k (first x)
        v (second x)]
        
    (case k
      1 [cnt (updateCounti m v true)]
      2 [cnt (updateCounti m v false)]
      3  [(conj cnt  (if (> (count (get m (format "count-%d" v)  #{}) ) 0)    1 0) )m])))
      
  

(defn freqQuery [queries](first(r/fold (r/monoid folder (constantly [[] {}])) queries)))
; (defn freqQuery [queries](r/fold (r/monoid folder (constantly [[] {}])) queries))
  

(freqQuery [[ 3 4]
            [ 1 1003]
            [ 1 10000029]
            [ 2 1003]
            [ 2 10000029]
            [ 1 10000029]
            [ 3 1]
            [ 3 2]])
            
  
