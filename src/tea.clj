(ns async-tea-party.core
  (:require [clojure.core.async :as async]))

(def google-tea-service-chan (async/chan 10))
(def yahoo-tea-service-chan (async/chan 10))

(defn random-add []
  (reduce + (conj [] (repeat (rand-int 100000) 1))))

(defn request-google-tea-service []
  (async/go
    (random-add)
    (async/>! google-tea-service-chan
              "tea compliments of google")))

(defn request-yahoo-tea-service []
  (async/go
    (random-add)
    (async/>! yahoo-tea-service-chan
              "tea compliments of yahoo")))

(defn request-tea []
  (request-google-tea-service)
  (request-yahoo-tea-service)
  (let [[v] (async/alts!!
                       [google-tea-service-chan
                        yahoo-tea-service-chan])]
              v))
(request-tea)
;; (async/go)
