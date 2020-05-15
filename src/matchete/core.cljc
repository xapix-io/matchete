(ns matchete.core
  (:require [matchete.matcher :as m]
            [clojure.walk :as walk]))

(defn- find-vars [P]
  (let [vars (atom #{})]
    (walk/postwalk (fn [x]
                     (when (or (m/binding? x) (m/memo-binding? x))
                       (swap! vars conj x))
                     x)
                   P)
    (vec @vars)))

(defn matches [pattern data]
  (if (fn? pattern)
    (pattern data)
    ((m/matcher pattern) data)))

(defn match? [pattern data]
  (boolean (seq (matches pattern data))))

(defmacro pdefn* [s body]
  (when (seq body)
    (let [[[P expr-body] & body] body
          vars (find-vars P)]
      `(let [sm# (matches (quote ~P) ~s)]
         (if sm#
           (for [{:syms ~vars} sm#]
             ~expr-body)
           (pdefn* ~s ~body))))))

(defmacro pdefn [name & body]
  (let [args-s (gensym)]
    `(defn ~name [& ~args-s]
       (pdefn* ~args-s ~body))))
