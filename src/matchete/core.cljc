(ns matchete.core
  (:require [matchete.matcher :as m]))

(defn matches [pattern data]
  (if (fn? pattern)
    (pattern data)
    ((m/matcher pattern) data)))

(defn match? [pattern data]
  (boolean (seq (matches pattern data))))

(def matcher m/matcher)

(defmacro defn* [patterns s body]
  (if (seq body)
    (let [[[P expr-body] & body] body
          vars (vec (m/find-bindings P))]
      `(let [sm# (matches (quote ~P) ~s)]
         (if (seq sm#)
           (for [{:syms ~vars} sm#]
             ~expr-body)
           (defn* ~patterns ~s ~body))))
    `(throw (ex-info "Can not find declaration that satisfy the arguments" {:arguments ~s
                                                                            :patterns (quote ~patterns)}))))

(defmacro fn-match
  {:style/indent :fn}
  [& sigs]
  (let [name (if (symbol? (first sigs)) (list (first sigs)) ())
        sigs (if (seq name) (next sigs) sigs)
        patterns (map first sigs)
        args-s (gensym)]
    (with-meta
      `(fn ~@name [& ~args-s]
         (defn* ~patterns ~args-s ~sigs))
      (meta &form))))

(defmacro defn-match
  {:style/indent :defn}
  [name & fdecl]
  (list 'def name
        (cons `fn-match fdecl)))
