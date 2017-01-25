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
    :generic symbol?
    :predicate (s/or
                 :fn fn?
                 :spec #(s/spec? %))
    :spec-ref keyword?))

(s/def ::product
  (s/or
    :empty-prod keyword?
    :prod (s/cat
            :variant keyword?
            :fields (s/+ ::field))))

(s/def ::adt
  (s/cat :type keyword?
         :generics (s/* symbol?)
         :products (s/* ::product)))


;; if generics then rename type keyword
;; if generics then substitute generic symbols for concrete types
;;   For each :prod, for each *generic* field, replace generic with spec or pred depending on type
;; sdef with the type name
;; or of each product
;; use tuple for product with fields
(defn adt
  "Returns a clojure spec for the ADT"
  [definition & gs]
  (let [{:keys [::type ::generics ::products]} (s/conform ::adt definition)
        type (if (some? generics)
               (keyword (namespace type) (apply str (name type) "-" (map str gs)))
               type)
        products (if (some? generics)
                   (substitute products generics gs)
                   products)]
    [type
     products]
    ))

(defn sdef
  [x y])

(defn sor
  [& xs])

(defn replace
  [lookup field]
  (println "in replace")
  (match field
         [:generic sym] (let [t (lookup sym)]
                          (if (s/spec? t)
                            [:spec-ref t]
                            [:predicate t]))
         :else field))

(defn replace-generics
  [conformed types]
  (let [lookup (zipmap (conformed :generics) types)]
    (reduce
      (fn [m [p f]]
        (update-in m [:products p 1 :fields f] (partial replace lookup)))
      (dissoc conformed :generics)
      (for [p (range 0 (count (conformed :products)))
            f (range 0 (count (get-in conformed [:products p 1 :fields])))]
        [p f]))))


(defmacro gen-tuple
  [kw fields]
  `(s/tuple (is ~kw) ~@fields))

(defn or-body
  [replaced]
  (update replaced :products (fn [ps]
                               (apply concat
                                      (for [p ps]
                                        (match p
                                               [:empty-prod kw] [kw kw]
                                               [:prod {:variant kw :fields fs}] [kw (gen-tuple kw (seq fs))]))))))

(defmacro adt-spec
  [type products]
  `(s/def ~type (s/or ~@products)))

(defn adt
  [definition & gs]
  (let [{:keys [type products]} (->
                                  (s/conform ::adt definition)
                                  (replace-generics gs)
                                  (or-body))]
    (adt-spec type (seq products))))

;(defmacro adt-spec
;  [type]
;  (let [{:keys [::type ::generics ::products]} (s/conform ::adt definition)
;        type (if (some? generics)
;               (keyword (namespace type) (apply str (name type) "-" (map str gs)))
;               type)
;        products (if (some? generics)
;                   (substitute products generics gs)
;                   products)]
;    `(s/def ~type
;       (s/or
;         ~@(apply concat
;                  (for [c [:red :green :blue]]
;                    [c (is c)]))))))




(def tree-result
  #:adt-spec.core{:type     :adt-spec.core/tree,
                  :generics ['a],
                  :products [[:empty-prod :empty]
                             [:prod #:adt-spec.core{:variant :leaf,
                                                    :fields  [[:adt-spec.core/generic 'a]]}]
                             [:prod #:adt-spec.core{:variant :node,
                                                    :fields  [[:adt-spec.core/spec :adt-spec.core/tree]
                                                              [:adt-spec.core/spec :adt-spec.core/tree]]}]]})

(def tree
  [::tree 'a
   :empty
   [:leaf 'a]
   [:node ::tree ::tree]])

(def point
  [::point
   [:point double? double? double?]
   [:point {:x double? :y double? :z double?}]])

(def generic-list
  [::list 'a
   :nil
   [:cons 'a ::list]])

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
  [::temp
   [:kelvin (s/and double? pos?)]
   [:celcius (s/and double? #(> 273.15))]
   [:farenheit (s/and double? #(> 459.67))]])

