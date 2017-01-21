(ns adt-spec.core
  (:require [clojure.spec :as s]
            [clojure.core.match :refer [match]]))

(defn is
  [v]
  #(= % v))

(defn tree-of
  "Returns a spec for a tree of type t? where t? is a spec."
  [t?]
  (s/or
    ::empty (s/tuple (is :empty))
    ::leaf (s/tuple (is :leaf) t?)
    ::node (s/tuple (is :node) ::tree ::tree)))

(defn maybe-of
  [t?]
  (s/or
    ::none (s/tuple (is :none))
    ::some (s/tuple (is :some) t?)))

(defn result-of
  [ok? err?]
  (s/or
    ::error (s/tuple (is :error) err?)
    ::ok (s/tuple (is :ok) ok?)))

;(sum ::tree
;     (prod ::empty)
;     (prod ::leaf 'a)
;     (prod ::node ::tree ::tree))

(s/def ::field
  (s/or
    ::generic symbol?
    ::predicate fn?
    ;;::spec #(s/spec? %)
    ::spec keyword?
    ))

(s/def ::product
  (s/or
    :empty-prod keyword?
    :prod (s/cat
            ::variant keyword?
            ::fields (s/+ ::field))))

(s/def ::adt
  (s/cat ::type keyword?
         ::generics (s/* symbol?)
         ::products (s/* ::product)))

(defn adt
  "Returns a clojure spec for the ADT"
  [definition & generics]
  )

(def tree
  [::tree 'a
   :empty
   [:leaf 'a]
   [:node ::tree ::tree]])

(def point
  [::point
   [:point double? double? double?]
   [:point {:x double? :y double? :z double?}]])

(def list
  (partial adt [::list 'a
                :nil
                [:cons 'a ::list]]))

(list int?)                                                 ;; -> spec of List<Long>

(def maybe
  [::maybe 'a
   :none
   [:some 'a]])

(def result
  [::result 'o 'e
   [:ok 'o]
   [:err 'e]])

(def color
  [::color
   :red
   :blue
   :green])

(def temperature
  (adt [::temp
        [:kelvin (s/and double? pos?)]
        [:celcius (s/and double? #(> 273.15))]
        [:farenheit (s/and double? #(> 459.67))]]))

