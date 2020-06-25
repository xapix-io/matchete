(ns matchete.json-schema
  (:require [clojure.string :as string]))

(defn subschema-type [_path {:strs [$id type $ref $defs definitions enum const anyOf allOf oneOf not default]
                             :as schema}]
  (cond
    (#{{} true} schema)                  :allow-all
    (= false schema)                     :disallow-all
    $defs                                :$defs
    definitions                          :definitions
    $id                                  :id
    (some some? [anyOf allOf oneOf not]) :combination
    enum                                 :enum
    const                                :const
    $ref                                 :ref
    (vector? type)                       :types
    type                                 (case type
                                           "null"    :nil
                                           "boolean" :boolean
                                           "string"  :string
                                           "integer" :integer
                                           "number"  :number
                                           "array"   :array
                                           "object"  :object)
    default                              :allow-all))

(defmulti json-schema->pattern subschema-type)

(defmethod json-schema->pattern :allow-all [path _]
  [:and {:ref path} '?_])

(defmethod json-schema->pattern :disallow-all [path _]
  [:not {:ref path} '?_])

(defmethod json-schema->pattern :id [path {:strs [$id] :as schema}]
  (let [pattern (json-schema->pattern path (dissoc schema "$id"))]
    (update-in pattern [1] merge {:id $id :ref path})))

(defmethod json-schema->pattern :$defs [path {:strs [$defs] :as schema}]
  (let [$defs (map (fn [[n p]]
                     (let [path (conj path "$defs" n)]
                       [:define {:id path} (json-schema->pattern path p)]))
                   $defs)]
    (vec (concat (list :and {})
                 $defs
                 (list (json-schema->pattern path (dissoc schema "$defs")))))))

(defmethod json-schema->pattern :definitions [path {:strs [definitions] :as schema}]
  (let [$defs (map (fn [[n p]]
                     (let [path (conj path "definitions" n)]
                       [:define {:id path} (json-schema->pattern path p)]))
                   definitions)]
    (vec (concat (list :and {})
                 $defs
                 (list (json-schema->pattern path (dissoc schema "definitions")))))))

(defmethod json-schema->pattern :enum [path {:strs [enum]}]
  (let [path (conj path "enum")]
    (vec (concat [:or {:ref path}]
                 (map-indexed #(vector :value {:ref (conj path %1)} %2) enum)))))

(defmethod json-schema->pattern :const [path {:strs [const]}]
  [:value {:ref (conj path "const")} const])

(defn normalize-ref [ref]
  (mapv (comp #(string/replace % #"~0" "~")
              #(string/replace % #"~1" "/"))
        (string/split ref #"/")))

(defmethod json-schema->pattern :ref [path {:strs [$ref]}]
  [:ref {:ref path :origin $ref} (normalize-ref $ref)])

(defmethod json-schema->pattern :types [path {:strs [type] :as schema}]
  (vec (concat (list :or {})
               (map (fn [t]
                      (let [res (json-schema->pattern path (assoc schema "type" t))]
                        (update-in res [1] dissoc :ref)))
                    type))))

(defmethod json-schema->pattern :nil [_ _]
  nil)

(defmethod json-schema->pattern :boolean [_ _]
  [:predicate {} boolean?])

(defmethod json-schema->pattern :string [_ {:strs [minLength maxLength #_pattern #_format]}]
  (let [base [:predicate {} string?]
        length (when (or minLength maxLength)
                 [:predicate {} #(<= (or minLength 0) (count %) (or maxLength ##Inf))])]
    (if length
      [:and {} base length]
      base)))

(defn numeric-validator [{:strs [multipleOf minimum maximum exclusiveMinimum exclusiveMaximum]}]
  (when (some some? [multipleOf minimum maximum exclusiveMinimum exclusiveMaximum])
    [:predicate {}
     (every-pred
      (if multipleOf #(zero? (mod % multipleOf)) (constantly true))
      (cond
        minimum #(>= % minimum)
        exclusiveMinimum #(> % exclusiveMinimum)
        :else (constantly true))
      (cond
        maximum #(<= % maximum)
        exclusiveMaximum #(< % exclusiveMaximum)
        :else (constantly true)))]))

(defmethod json-schema->pattern :integer [_ schema]
  (let [base [:predicate {} integer?]
        validator (numeric-validator schema)]
    (if validator
      [:and {} base validator]
      base)))

(defmethod json-schema->pattern :number [_ schema]
  (let [base [:predicate {} number?]
        validator (numeric-validator schema)]
    (if validator
      [:and {} base validator]
      base)))

(defmethod json-schema->pattern :array [path {:strs [items minItems maxItems uniqueItems additionalItems] :as schema}]
  (if (= false additionalItems)
    (json-schema->pattern path (-> schema (assoc "maxItems" (count items)) (dissoc "additionalItems")))
    (let [additional-items (cond
                             (map? additionalItems)
                             ['& [:each {} (json-schema->pattern (conj path "additionalItems") additionalItems)]]

                             (= false additionalItems)
                             ['& '?_])
          items (vec (cond
                       (sequential? items)
                       (concat [:seq {:ref path}]
                               (map-indexed #(json-schema->pattern (conj path "items" %1) %2) items)
                               additional-items)

                       (map? items)
                       (conj [:each {:ref (conj path "items")}]
                             (json-schema->pattern (conj path "items") items))

                       :else
                       (concat [:seq {:ref path}]
                               additional-items)))]
      (vec (concat [:and {}] [items]
                   (when (some some? [minItems maxItems])
                     [[:predicate {} #(<= (or minItems 0) (count %) (or maxItems ##Inf))]])
                   (when uniqueItems
                     [[:predicate {} distinct?]]))))))

(defmethod json-schema->pattern :object [path {:strs [properties]}]
  (vec (concat [:map {:ref path}]
               (map (fn [[k vs]]
                      [k (json-schema->pattern (conj path "properties" k) vs)])
                    properties))))

(defmethod json-schema->pattern :combination [path schema]
  (let [to-combine (filter #(not-empty (get schema %)) ["anyOf" "allOf" "oneOf" "not"])
        combos (map #((get-method json-schema->pattern %) path schema)
                    to-combine)]
    (vec (if (= 1 (count combos))
           (first combos)
           (concat (list :and {}) combos)))))

(defmethod json-schema->pattern "anyOf" [path {:strs [anyOf]}]
  (concat (list :or {})
          (map-indexed #(json-schema->pattern (conj path "anyOf" %1) %2) anyOf)))

(defmethod json-schema->pattern "allOf" [path {:strs [allOf]}]
  (concat (list :and {})
          (map-indexed #(json-schema->pattern (conj path "allOf" %1) %2) allOf)))

(defmethod json-schema->pattern "oneOf" [path {:strs [oneOf]}]
  (concat (list :only-one {})
          (map-indexed #(json-schema->pattern (conj path "oneOf" %1) %2) oneOf)))

(defmethod json-schema->pattern "not" [path {:strs [not]}]
  [:not (json-schema->pattern (conj path "not") not)])
