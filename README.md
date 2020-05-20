# matchete

Yet another pattern matching library for Clojure(Script).

## Data as a pattern

```clojure
(require '[matchete.core :refer [match? matches]])

;; constants
(match? 42 42) ;; => true
(match? 42 24) ;; => false

;; collections
(match? [1 2 3] [1 2 3]) ;; => true
(match? {:x 1} {:x 1}) ;; => true

;; nested collections
(match? {:vec [1 2 3]
         :map {:x 1
               :y 2}}
        {:vec [1 2 3]
         :map {:x 1
               :y 2}}) ;; => true

;; sequential matchers
;; elements in sequence are matches while element present in sequence
(match? [1 2 3] [1 2 3 42]) ;; => true, because first three elements in vector matches pattern
(match? [1 2 3] [1 2]) ;; => false, because pattern require third element to match against 3

;; same for hash maps
(match? {:x 1} {:x 1 :y 2}) ;; => true
(match? {:x 1 :y 2} {:x 1}) ;; => false
```

## Extract bindings

```clojure
;; binding is a symbol that starts with '?' is a logical variable to extract values from matched data
(matches '{:x ?x :y ?y} {:x 1 :y 2}) ;; => ({?x 1 ?y 2})

(matches '{:x ?n :y ?n} {:x 1 :y 1}) ;; => ({?n 1})
(matches '{:x ?n :y ?n} {:x 1 :y 2}) ;; => ()

;; memo binding is a symbol that starts with '!' is a collection binding to store matched data
;; item order is not guaranteed
(matches '{:x !n :y !n} {:x 1 :y 2}) ;; => ({!n [1 2]})

;; '_' symbol is a placeholder to match anything without binding value
(matches '{:x _ :y _ :z ?z} {:x 1 :y 2 :z 3}) ;; => ({?z 3})

;; binding can be used as a key in map pattern
(matches '{?x 1} {:x 1}) ;; => ({?x :x})

;; 'matches' returns a list because binding as a key can match multiple times
(matches '{?x 1} {:x 1 :y 1}) ;; => ({?x :x} {?x :y})

;; more sofisticated examples
(matches
 '{?x ?y
   ?y ?x
   ?z [?x ?y]}
 {:x :y
  :y :x
  :z [:x :y]
  :q [:y :x]})
;; => ({?x :x, ?y :y, ?z :z} {?x :y, ?y :x, ?z :q})
```

## Control sequence

```clojure
;; by default sequential pattern will match if the matched sequence has the same length as the pattern
(match? '[?x 42 "qwe"] [:foo 42 "qwe" :tail-element]) ;; => false

;; tail matcher can be added to the end of the pattern to override this behaviour
(match? '[?x 42 "qwe" & _] [:foo 42 "qwe" :tail-element]) ;; => true

;; to extract and match the tail of sequence it is possible to use '&
;; This is working as in clojure destructuring pattern
(matches '[?x ?y & ?z] [1 2 3 4 5]) ;; => ({?x 1 ?y 2 ?z (3 4 5)})

;; `cat` control sequence can wrap multiple patterns in a subsequence of matches
(matches '(cat {:id ?id :name ?name} ;; match a map with :id and :role keys
               {:role :admin}        ;; match a map with :role key
               ?user)                ;; match any data and fill ?user binding
         {:id 1
          :name "Dave"
          :role :admin
          :company "X"})
;; => ({?id 1
;;      ?name "Dave"
;;      ?user {:id 1
;;             :name "Dave"
;;             :role :admin
;;             :company "X"}})

;; `alt` control sequence
(matches '(alt {:x ?x} {:y ?x} {:z ?x})
         ;; {:x 1} ;; => ({?x 1})
         ;; {:y 2} ;; => ({?x 2})
         {:z 3} ;; => ({?x 3})
         )
```

## Functions with pattern-matching dispatch

```clojure
;; `defn-match` and `fn-match` macros can be used to create a function with pattern-based dispatch
;; they are similar to their clojure siblings `defn` and `fn`
;; there are two main difference:
;;   * result is always a list with zero or many elements
;;   * patterns checked from top to bottom and the first not empty results of applying `matches` function will be used to create local bindings to run the body associated with that pattern

(defn-match foo
  ([0] 1)
  ([1] 0)
  ([{?x 1}] (first (foo ?x)))
  ([?x] (str "this is X: " ?x)))

(foo 0) ;; => (1)
(foo 1) ;; => (0)
(foo "`I'm X`") ;; => ("this is X: `I'm X`")
(foo {"I'm X" 1
      "I'm also X" 1}) ;; => ("this is X: I'm X" "this is X: I'm also X")
```
