(ns matchete.core
  (:require [matchete.matcher :as m]))

(defn- find-vars [P]
  (cond
    (or (m/binding? P)
        (m/memo-binding? P))
    (list P)

    (sequential? P)
    (mapcat find-vars P)

    (map? P)
    (mapcat find-vars (seq P))

    :else ()))

(defn matches [pattern data]
  (if (fn? pattern)
    (pattern data)
    ((m/matcher pattern) data)))

(defn match? [pattern data]
  (boolean (seq (matches pattern data))))

(defmacro pdefn* [s body]
  (when (seq body)
    (let [[[P expr-body] & body] body
          vars (vec (find-vars P))]
      `(let [sm# (matches (quote ~P) ~s)]
         (if sm#
           (for [{:syms ~vars} sm#]
             ~expr-body)
           (pdefn* ~s ~body))))))

(defmacro pdefn [name & body]
  (let [args-s (gensym)]
    `(defn ~name [& ~args-s]
       (pdefn* ~args-s ~body))))
