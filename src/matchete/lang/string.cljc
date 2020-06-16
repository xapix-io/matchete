(ns matchete.lang.string
  (:require [clojure.string :as string]
            [matchete.lang :as ml]
            [matchete.lang.core :as mlcore]))

(defn- make-pattern
  ([p] (make-pattern p nil))
  ([p args]
   (mlcore/and
    mlcore/string?
    (reify ml/Pattern
      (matches [_ preconditions data]
        (when (apply p (list* data args))
          (list preconditions)))))))

(def blank?
  (make-pattern string/blank?))
(defn ends-with? [& args]
  (make-pattern string/ends-with? args))
(defn includes? [& args]
  (make-pattern string/includes? args))
(defn starts-with? [& args]
  (make-pattern string/starts-with? args))
