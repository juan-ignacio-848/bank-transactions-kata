(ns time.interval
  (:require [java-time :as time]))

(defn interval [time span unit]
  (let [t (time/instant time)
        start (.. t (minus span unit))
        end (.. t (plus span unit))]
    {:start start :end end}))

(defn within-interval [time interval]
  (let [{:keys [start end]} interval]
    (or (and (.. time (isAfter start)) (.. time (isBefore end)))
        (= time start)
        (= time end))))