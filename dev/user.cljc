(ns user)

(defmacro predicates->pattern-defns [& preds]
  `(do ~@(for [p preds]
           `(defn ~(symbol (name p)) [& args#]
              (make-pattern ~p args#)))))

(defmacro predicates->pattern-defs [& preds]
  `(do ~@(for [p preds]
           `(def ~(symbol (name p))
              (make-pattern ~p)))))
