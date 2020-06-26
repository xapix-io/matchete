(ns matchete.data-form
  (:require [matchete.core :as mc]
            [sci.core :as sci]))

(defn pattern-type [_ P]
  (when
      (vector? P)
    (first P)))

(defn save-pattern! [{:keys [id]} named P]
  (when (some? id)
    (swap! named assoc id P))
  P)

(defmulti ->pattern pattern-type)

(defmethod ->pattern :default [_ value] value)

(defmethod ->pattern :value [_ [_ _ value]]
  value)

(defmethod ->pattern :map [named [_ opts & entries]]
  (let [{optional-keys true
         required-keys false} (group-by #(boolean (get-in % [1 :optional])) entries)
        make-map-pattern (fn [kvs]
                           (into {}
                                 (map (fn [[k _ v]]
                                        [(->pattern named k)
                                         (->pattern named v)]))
                                 kvs))
        optional-map-pattern (mc/open-map (make-map-pattern optional-keys))
        required-map-pattern (make-map-pattern required-keys)]
    (cond
      (and optional-keys required-keys)
      (save-pattern! opts named (mc/and optional-map-pattern required-map-pattern))

      optional-keys
      (save-pattern! opts named optional-map-pattern)

      required-keys
      (save-pattern! opts named required-map-pattern))))

(defmethod ->pattern :seq [named [_ opts & items]]
  (save-pattern! opts named (into [] (map #(->pattern named %)) items)))

(defmethod ->pattern :set [named [_ opts & items]]
  (save-pattern! opts named (into #{} (map #(->pattern named %)) items)))

(defmethod ->pattern :or [named [_ opts & PS]]
  (save-pattern! opts named (apply mc/or (mapv (partial ->pattern named) PS))))

(defmethod ->pattern :one-of [named [_ opts & PS]]
  (save-pattern! opts named (apply mc/only-one (map (partial ->pattern named) PS))))

(defmethod ->pattern :and [named [_ opts & PS]]
  (save-pattern! opts named (apply mc/and (map (partial ->pattern named) PS))))

(defmethod ->pattern :not [named [_ opts P]]
  (save-pattern! opts named (mc/not (->pattern named P))))

(defmethod ->pattern :if [named [_ opts & PS]]
  (save-pattern! opts named (apply mc/if* (map (partial ->pattern named) PS))))

(defmethod ->pattern :scan [named [_ opts & PS]]
  (save-pattern! opts named (apply mc/scan (map (partial ->pattern named) PS))))

(defmethod ->pattern :each [named [_ opts & PS]]
  (save-pattern! opts named (apply mc/each (map (partial ->pattern named) PS))))

(defmethod ->pattern :some [named [_ opts & PS]]
  (save-pattern! opts named (apply mc/some (map (partial ->pattern named) PS))))

(defmethod ->pattern :update-at [named [_ opts dest f]]
  (save-pattern! opts named (mc/update-at dest (->pattern named f))))

(defmethod ->pattern :pred [named [_ opts & args]]
  (save-pattern! opts named (apply mc/predicate (map (partial ->pattern named) args))))

(defmethod ->pattern :guard [named [_ opts f]]
  (save-pattern! opts named (mc/guard (->pattern named f))))

(defmethod ->pattern :reshape-by [named [_ opts f P]]
  (save-pattern! opts named (mc/reshape-by (->pattern named f) (->pattern named P))))

(defmethod ->pattern :with-refs [named [_ opts bindings P]]
  (doall (map #(save-pattern! {:id (first %)} named (->pattern (second %)))
              (partition 2 bindings)))
  (save-pattern! opts named (->pattern named P)))

(defmethod ->pattern :fn [_ [_ _ form]]
  (sci/eval-string (str form) {:preset :termination-safe}))

(defmethod ->pattern :ref [named [_ _opts reference]]
  (with-meta
    (fn [data ms]
      ((mc/pattern (get @named reference)) data ms))
    {:pattern true}))

(defn inject-opts [P]
  (if (vector? P)
    (let [[t & P] P
          opts (if (map? (first P))
                 (first P)
                 {})
          P (map inject-opts (if (map? (first P))
                               (rest P)
                               P))]
      (into [] (cons t (cons opts P))))
    P))

(defn make-pattern [P]
  (let [P (inject-opts P)]
    (->pattern (atom {}) P)))
