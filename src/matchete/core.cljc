(ns matchete.core
  (:require [matchete.matcher :as m]))

(defn find-vars [P]
  (cond
    (and (symbol? P)
         (not= '_ P))
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

(def matcher m/matcher)

(defmacro defn* [patterns s body]
  (if (seq body)
    (let [[[P expr-body] & body] body
          vars (vec (find-vars P))]
      `(let [sm# (matches (quote ~P) ~s)]
         (if (seq sm#)
           (for [{:syms ~vars} sm#]
             ~expr-body)
           (defn* ~patterns ~s ~body))))
    `(throw (ex-info "Can not find declaration that satisfy the arguments" {:arguments ~s
                                                                            :patterns (quote ~patterns)}))))

(defmacro fn-match
  ""
  {:arglists '([name? [param-patterns* ] exprs*]
               [name? ([param-patterns* ] exprs* )+ ])}
  [& sigs]
  (let [name (if (symbol? (first sigs)) (list (first sigs)) ())
        sigs (if (seq name) (next sigs) sigs)
        sigs (if (vector? (first sigs))
               (list sigs)
               (if (seq? (first sigs))
                 sigs
                 (throw (ex-info "Parameter declaration is missing or not a vector" {:form sigs}))))
        patterns (map first sigs)
        args-s (gensym)]
    (with-meta
      `(fn ~@name [& ~args-s]
         (defn* ~patterns ~args-s ~sigs))
      (meta &form))))

(defmacro defn-match
  ""
  {:arglists '([name doc-string? attr-map? [param-patterns*] exprs* ]
               [name doc-string? attr-map? ([param-patterns*] exprs* )+ ])}
  [name & fdecl]
  (let [m (if (string? (first fdecl))
            {:doc (first fdecl)}
            {})
        fdecl (if (string? (first fdecl))
                (next fdecl)
                fdecl)
        m (if (map? (first fdecl))
            (conj m (first fdecl))
            m)
        fdecl (if (map? (first fdecl))
                (next fdecl)
                fdecl)
        fdecl (if (vector? (first fdecl))
                (list fdecl)
                fdecl)
        m (if (map? (last fdecl))
            (conj m (last fdecl))
            m)
        fdecl (if (map? (last fdecl))
                (butlast fdecl)
                fdecl)]
    (list 'def (with-meta name m)
          (cons `fn-match fdecl))))
