# matchete

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
;; by default sequential pattern will match if the matched sequence is not shorter than pattern
(matches '[?x 42 "qwe"] [:foo 42 "qwe" :tail-element]) ;; => ({?x :foo})

;; wrapping the pattern into control sequence can override this behaviour
(matches '(exact [?x 42 "qwe"]) [:foo 42 "qwe" :tail-element]) ;; => ()
(matches '(exact [?x 42 "qwe" :tail-element]) [:foo 42 "qwe" :tail-element]) ;; => ({?x :foo})

;; `cat` control sequence can wrap multiple patterns in a subsequence of matches
(matches '(cat {:id ?id :name ?name} ;; match a map with :id and :role keys
               {:role :admin}        ;; match a map with :role key
               ?user)                ;; match any data and fill ?user binding
         {:id 1
          :name "Dave"
          :role :admin
          :company "X"})
;; => ({?id 1
        ?name "Dave"
        ?user {:id 1
               :name "Dave"
               :role :admin
               :company "X"}})

;; `alt` control sequence
(matches '(alt {:x ?x} {:y ?x} {:z ?x})
         ;; {:x 1} ;; => ({?x 1})
         ;; {:y 2} ;; => ({?x 2})
         {:z 3} ;; => ({?x 3})
         )
```
