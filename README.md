# matchete

Yet another pattern matching library for Clojure(Script).

## Using

```clojure
(require '[matchete.core :as m])
```

## Match data using data as a pattern

```clojure
(m/match? 42 42) ;; => true
(m/match? "42" "24") ;; => false

;; sequences
(m/match? [1 2 3] [1 2 3]) ;; => true
(m/match? '(1 2 3) [1 2 3]) ;; => true

(m/match? [1 2 3] [1 2 3 4]) ;; => false because pattern expects exactly 3 elements

;; to override this behaviour tail destructuring pattern can be used
(m/match? [1 2 3 & _] [1 2 3 4]) ;; => true, `_` is a placeholder here that will match to any provided data

;; hash-maps
(m/match? {:id 123 :name "Alise"} {:id 123 :name "Alise" :lastname "Cooper"}) ;; => true
(m/match? {:id 123 :name "Alise"} {:id 123 :lastname "Cooper"}) ;; => false because `:name` key is missing
```

## Extract data

There are three types of special symbols that can be used in a pattern:

* data bindings - symbols starts with '?'
* memo bindings - symbols starts with '!'
* named rule - symbols starts with '$'

### Data Binding

```clojure
(m/matches '?user {:id 1 :name "Bob"}) ;; => '({?user {:id 1 :name "Bob"}})
(m/matches '{:id ?user-id :name ?user-name}
            {:id 1        :name "Bob"}) ;; => '({?user-id 1 ?user-name "Bob"})

(m/matches '[1 ?two 3 & [?four & _]]
            [1 2    3    4       5 6]) ;; => '({?two 2 ?four 4})

(m/matches '{:vector [_       {:id ?id}]}
            {:vector [{:id 1} {:id 2}]}) ;; => '({?id 2})
```

### Memo Binding

```clojure
(m/matches '[{:id !ids} {:id !ids} {:id !ids}]
            [{:id 1}    {:id 2}    {:id 3}]) ;; => '({!ids [1 2 3]})
```

## Control sequences

### `and` combinator

### `or` combinator

### `scan`

### `scan-indexed`

Expects two patterns:

  1. index matcher (index in sequences and key in hash-maps)
  1. value matcher

```clojure
(m/matches '(scan-indexed !path (scan-indexed !path (scan-indexed !path ?node)))
            [{:id 1
              :user {:name "Alise"
                     :role :admin}
              :actions [{:type :login}]}])
;; => '({!path [0 :user :name] ?node "Alise"}
;;      {!path [0 :user :role] ?node :admin}
;;      {!path [0 :actions 0]  ?node {:type :login}})
```

### Named rule

```clojure
(m/matches '(rule $children (scan-indexed !path (or $children ?leaf)))
            [{:id 1
              :user {:name "Alise"
                     :role :admin}
              :actions [{:type :login}]}])
;; => '({!path [0 :id]              ?leaf 1}
;;      {!path [0 :user :name]      ?leaf "Alise"}
;;      {!path [0 :user :role]      ?leaf :admin}
;;      {!path [0 :actions 0 :type] ?leaf :login})

;; rules can be precompiled
(let [rules {'$children (m/matcher '(scan-indexed !path (or $children ?leaf)))}]
  (m/matches '$children rules
             [{:id 1
               :user {:name "Alise"
                      :role :admin}
               :actions [{:type :login}]}]))
;; => '({!path [0 :id]              ?leaf 1}
;;      {!path [0 :user :name]      ?leaf "Alise"}
;;      {!path [0 :user :role]      ?leaf :admin}
;;      {!path [0 :actions 0 :type] ?leaf :login})
```
