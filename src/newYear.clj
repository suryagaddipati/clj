(defn minimumBribes  [q] (let [len (count q)]
                          ( loop [idx 0 p 0 m 0] 
                            (if (>= idx len) (if (= 0 (+ p m) ) p "Too chaotic")
                               (let [ v (nth q idx)
                                      diff (- v (inc idx)) 
                                      newIdx (if (> diff 2) len (inc idx)) 
                                      isJumpF (>  diff 0)
                                      newP (if isJumpF (+ p diff) p) 
                                      newM (if-not isJumpF (+ m diff) m)] 
                                 (recur newIdx newP  newM))))))
                              
                                                       
                           
                          
  
 
( minimumBribes [2 1 5 3 4])
; ( minimumBribes [2 5 1 3 4])
                 
                  
  

