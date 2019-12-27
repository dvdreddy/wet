(ns wet.impl.parser
  (:require #?(:cljs [cljs.reader])
            [instaparse.core :as insta
             #?@(:clj [:refer [defparser]]
                 :cljs [:refer-macros [defparser]])]
            [wet.filters :as filters]
            [wet.impl.parser.grammar :as grammar]
            [wet.impl.parser.nodes :as nodes
             #?@(:cljs [:refer [Condition Else Filter ForLimit ForOffset
                                ForReversed Lookup When ObjectExpr]])])
  #?(:clj (:import (wet.impl.parser.nodes
                     Condition Else Filter ForLimit ForOffset
                     ForReversed Lookup When ObjectExpr))))

(def parse grammar/PARSED-GRAMMAR)

(defn- parse-template [& nodes] (nodes/->Template nodes))

(defn- parse-string [& nodes] (apply str nodes))

(defn- parse-condition-expr
  [constructor pred template & nodes]
  (let [conditions (->> nodes
                        (take-while (partial instance? Condition))
                        (cons (nodes/->Condition pred template)))
        else (when (instance? Else (last nodes)) (last nodes))]
    (constructor conditions else)))

(def ^:private parse-if (partial parse-condition-expr nodes/->If))

(def ^:private parse-unless (partial parse-condition-expr nodes/->Unless))

(defn- parse-case
  [val & nodes]
  (let [whens (take-while (partial instance? When) nodes)
        else (when (instance? Else (last nodes)) (last nodes))]
    (nodes/->Case val whens else)))

(defn- parse-assertion
  ([v] v)
  ([operand1 operator operand2]
   (nodes/->Assertion (:value operator) [operand1 operand2])))

(defn- parse-or [& predicates] (nodes/->PredicateOr predicates))

(defn- parse-and [& predicates] (nodes/->PredicateAnd predicates))

(defn- parse-object-expr [obj & filters] (nodes/->ObjectExpr obj filters))

(defn- parse-filter [name & args] (nodes/->Filter name args))

(defn- parse-for-opts
  [& nodes]
  (letfn [(find-node [t] (first (filter (partial instance? t) nodes)))]
    (nodes/map->ForOpts {:limit (:val (find-node ForLimit))
                         :offset (:val (find-node ForOffset))
                         :reversed? (some? (find-node ForReversed))})))

(defn- parse-for
  ([var collection template]
   (parse-for var collection nil template))
  ([var collection for-opts template]
   (nodes/->For var collection for-opts template)))

(defn- parse-lookup [name & nodes] (nodes/->Lookup name nodes))

(defn- read-string*
  [s]
  #?(:clj (read-string s)
     :cljs (cljs.reader/read-string s)))

(def ^:private transformer
  {:template parse-template
   :b identity
   :s identity
   ;; Data types
   :int read-string*
   :float read-string*
   :bool read-string*
   :sq-str-set identity
   :sq-str-escape read-string*
   :dq-str-set identity
   :dq-str-escape read-string*
   :string parse-string
   ;; Lookup
   :lookup parse-lookup
   :object-expr parse-object-expr
   :filter parse-filter
   :index nodes/->CollIndex
   ;; Assignment
   :capture nodes/->Capture
   :assign nodes/->Assign
   :increment nodes/->Increment
   :decrement nodes/->Decrement
   ;; Conditions
   :operator nodes/->Operator
   :assertion parse-assertion
   :predicate identity
   :else nodes/->Else
   :elsif nodes/->Condition
   :if parse-if
   :unless parse-unless
   :case parse-case
   :when nodes/->When
   :and parse-and
   :or parse-or
   ;; Iteration
   :for parse-for
   :for-opts parse-for-opts
   :for-limit nodes/->ForLimit
   :for-offset nodes/->ForOffset
   :for-reversed nodes/->ForReversed
   :break nodes/->Break
   :continue nodes/->Continue
   :range-start identity
   :range-end identity
   :range nodes/->IntRange
   ;; Raw
   :raw-block identity
   :raw-body identity})

(defn- transform
  [template]
  (insta/transform transformer template))

(defn parse-and-transform
  [template]
  (let [parsed-template (parse template)]
    (cond
      (insta/failure? parsed-template)
      (let [{:keys [text line column]} parsed-template
            error-message (str "Parse error: " text " (" line ":" column ")")
            error-context {:type :wet/parse-error
                           :wet.error/line line
                           :wet.error/column column
                           :wet.error/text text}]
        (throw (ex-info error-message error-context)))
      :else (transform parsed-template))))

(defn normalized-lookup-obj
  [{:keys [name fns] :as lookup-node}]
  (assert (instance? Lookup lookup-node)
          "Normalizing a non Lookup object not supported")
  ;; Code similar to the one from render logic
  ;; but we are just reconstructing stuff back
  (reduce
    (fn [res {lookup-key :key :as coll-index}]
      (cond (string? lookup-key)
            (str res "." lookup-key)

            (number? lookup-key)
            (str res "[" lookup-key "]")

            :else
            (str res "[" (normalized-lookup-obj lookup-key) "]")))
    name fns))


(defn analyse
  [transformed-template & {:as opts}]
  (let [nodes (->> transformed-template
                   (tree-seq
                     (fn [node] (satisfies? nodes/Parent node))
                     nodes/children))
        lookups (filter (partial instance? Lookup) nodes)
        filters (->> nodes
                     (filter (partial instance? Filter))
                     (map :name)
                     (distinct)
                     (group-by (partial contains? filters/CORE-FILTERS)))]
    (merge
      {:lookups (set (map :name lookups))
       :core-filters (set (get filters true))
       :custom-filters (set (get filters false))}
      (if (:detailed-lookup-info? opts)
        {:detailed-lookup-infos
         (->> nodes
              (filter #(and (instance? ObjectExpr %)
                            (instance? Lookup (:obj %))))
              (map (fn [{lookup :obj :keys [filters]
                         :as obj-expr}]
                     (merge
                       (zipmap [:start-idx :end-idx]
                               (insta/span obj-expr))

                       {:has-filters? (not (empty? filters))

                        :normalized-str
                        (normalized-lookup-obj lookup)}))))}))))
