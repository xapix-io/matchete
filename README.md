# matchete [![cljdoc badge](https://cljdoc.org/badge/io.xapix/matchete)](https://cljdoc.org/d/io.xapix/matchete/CURRENT) ![Check code style using clj-kondo](https://github.com/xapix-io/matchete/workflows/Check%20code%20style%20using%20clj-kondo/badge.svg?branch=master) ![Run tests for all environments](https://github.com/xapix-io/matchete/workflows/Run%20tests%20for%20all%20environments/badge.svg?branch=master)

Yet another pattern matching library for Clojure(Script).

## Using

<details><summary>leiningen</summary>
<p>

```
[io.xapix/matchete "1.1.0"]
```

</p>
</details>

<details><summary>boot</summary>
<p>

```
(set-env! :dependencies #(conj % [io.xapix/matchete "1.1.0"]))
```

</p>
</details>

<details><summary>deps.edn</summary>
<p>

```
{:deps {io.xapix/matchete {:mvn/version "1.1.0"}}}
```

</p>
</details>

```clojure
(require '[matchete.core :as m])

;; `match?` function returns true or false to indicate if the data matches the pattern
(m/match? '{:foo ?foo} {:foo 1}) ;; => true
(m/match? '{:bar ?bar} {:foo 1}) ;; => false

;; `matches` function returns lazy sequence with collected bindings
;; empty seq indicates not matched data
(m/matches '{:foo ?foo} {:foo 1}) ;; => '({?foo 1})
(m/matches '{:bar ?bar} {:foo 1}) ;; => '()

;; `matcher` function precompiles pattern into a function
(let [matcher (m/matcher '{:foo ?foo})]
  (matcher {:foo 1})) ;; => '({?foo 1})
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

data bindings can be used as a hash-map keys

```clojure
(m/matches '{?key ?value}
            {:foo "foo"
             :bar "bar"}) ;; => '({?key :foo ?value "foo"} {?key :bar ?value "bar"})

(m/matches '{?x "foo"
             ?y "bar"}
            {:key-1 "foo"
             :key-2 "foo"
             :key-3 "bar"}) ;; => '({?x :key-1 ?y :key-3} {?x :key-2 ?y :key-3})
```

### Memo Binding

Collect data into a vector. Order of appearence is not guaranteed.

```clojure
(m/matches '[{:id !ids} {:id !ids} {:id !ids}]
            [{:id 1}    {:id 2}    {:id 3}]) ;; => '({!ids [1 2 3]})
```

## Control sequences

### `not!` predicate

```
(m/matches '{:id (cat (not! 10) ?id)
             :name ?name}
           {:id 42
            :name "Alise"} ;; => '({?id 42 ?name "Alise"})
           ;; {:id 10
           ;;  :name "Bob"} ;; => '()
           )
```

### `cat` combinator

Each pattern will be applied to the same data combine data bindings into one result. Patterns can extend the result or add more sofisticated restrictions.

```clojure
(m/matches '(cat {:id ?id :name ?name} ?user)
            {:id 1
             :name "Alise"
             :lastname "Cooper"}) ;; => '({?id 1 ?name "Alise" ?user {:id 1 :name "Alise" :lastname "Cooper"}})
```

### `alt` combinator

Patterns combined by `alt` will be applied to the same data as long as one of them will match and the matches from that pattern will be the result of matching.

```clojure
(m/matches '(alt {:id ?id} {"id" ?id} {:userId ?id})
            {:id 1}        ;; => '({?id 1})
            ;; {"id" 2}    ;; => '({?id 2})
            ;; {:userId 3} ;; => '({?id 3})
            )
```

### `each`

#### `(each P)`

Sequentialy match elements of collection in order. Fail if any of elements can not match.

```clojure
(m/matches '(every (and %string? !elements))
           ["qwe" "rty" "uio"]) ;; => '({!elements ["qwe" "rty" "uio"]})

(m/matches '(every (and %string? !elements))
           ["qwe" 42 "uio"]) ;; => '()
```

#### `(each index-P value-P)`

2-arity version of `each` where first pattern will match against an index and second - match against value associated with that index.

### `scan`

#### `(scan P)`

Expects one pattern wich will be applied to each item of sequence or hash-map (item will be in the form of tuple: [key, value]).

```clojure
(m/matches '{:foo (scan [?id ?name])}
            {:foo [[1 "Alise"] [::empty] [3 "Bob"]]}) ;; => '({?id 1 ?name "Alise"} {?id 3 ?name "Bob"})'
```

#### `(scan index-P value-P)`

Expects two patterns:

  1. index matcher (index in sequences and key in hash-maps)
  1. value matcher

```clojure
(m/matches '(scan !path (scan !path (scan !path ?node)))
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
(m/matches '(def-rule $children (scan !path (alt $children ?leaf)))
            [{:id 1
              :user {:name "Alise"
                     :role :admin}
              :actions [{:type :login}]}])
;; => '({!path [0 :id]              ?leaf 1}
;;      {!path [0 :user :name]      ?leaf "Alise"}
;;      {!path [0 :user :role]      ?leaf :admin}
;;      {!path [0 :actions 0 :type] ?leaf :login})

;; rules can be precompiled
(let [rules {'$children (m/matcher '(scan !path (alt $children ?leaf)))}]
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
