(ns matchete.core
  (:require [matchete.matcher :as m]))

(defn matches [pattern data]
  (if (fn? pattern)
    (pattern data)
    ((m/matcher pattern) data)))

(defn match? [pattern data]
  (boolean (seq (matches pattern data))))
