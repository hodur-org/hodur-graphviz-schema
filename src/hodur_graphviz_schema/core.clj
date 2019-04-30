(ns hodur-graphviz-schema.core
  (:require [clojure.java.io :as io]
            [clojure.string :as string]
            [datascript.core :as d]))

(def header (slurp (io/resource "header.dot")))

(def footer (slurp (io/resource "footer.dot")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn ^:private cardinality-label
  [[from to]]
  (if (= from to)
    (if (= from 1) "" (str "[" from "]"))
    (str "[" from ".." to "]")))

(defn ^:private optional-label
  [optional]
  (if optional "?" ""))

(defn  ^:private type-label [{:keys [type/name] :as t} cardinality optional]
  (if name
    (str ": " name (cardinality-label cardinality) (optional-label optional))
    (if optional
      (str ": " (optional-label optional))
      "")))

(defn ^:private param-label
  [{:keys [param/name
           param/type
           param/cardinality
           param/optional]}]
  (str name (type-label type cardinality optional)))

(defn ^:private params-label
  [params]
  (if (not (empty? params))
    (str "("
         (->> params
              (map param-label)
              (string/join ", "))
         ")")
    ""))

(defn ^:private field-label
  [{:keys [field/name field/type
           field/cardinality
           field/optional
           param/_parent] :as field}]
  (str name
       (params-label _parent)
       (type-label type cardinality optional)
       "\\l"))

(defn ^:private field-list
  [fields]
  (reduce (fn [acc field]
            (str acc (field-label field)))
          "" fields))

(defn ^:private value-list
  [fields]
  (reduce (fn [acc {:keys [field/name]}] (str acc name "\\l")) "" fields))

(defn ^:private node-content
  [{:keys [type/name type/enum type/interface type/union field/_parent] :as t}]
  (cond
    interface (str "\\<\\<interface\\>\\>\\n" name "|" (field-list _parent))
    enum      (str "\\<\\<enum\\>\\>\\n" name "|" (value-list _parent))
    union     (str "\\<\\<union\\>\\>\\n" name "|" (value-list _parent))
    :else     (str name "|" (field-list _parent))))

(defn ^:private node-color
  [{:keys [graphviz/color]}]
  (if color
    (str "fillcolor=" color ";style=filled;")
    ""))

(defn ^:private parse-node
  [{:keys [type/name] :as t}]
  (str name " "
       "[label=\"{"
       (node-content t)
       "}\";"
       (node-color t)
       "]\n"))

(defn ^:private parse-nodes
  [types]
  (reduce (fn [c t]
            (str c (parse-node t)))
          "" types))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn ^:private edge-label-cardinality
  [[from to]]
  (if (= from to) (str from) (str from ".." to)))

(defn ^:private edge-label-name
  [name]
  (if name
    (str "label=\" " name "\";")
    ""))

(defn ^:private append-cardinality-label
  [label cardinality]
  (let [cardinality-str (edge-label-cardinality cardinality)]
    (if (= cardinality-str "1")
      label
      (str label
           (when label "\n")
           cardinality-str))))

(defn ^:private edge-label
  ([name src dst cardinality]
   (edge-label name src dst cardinality nil))
  ([name src dst cardinality extra]
   (if (= :user (:type/nature dst))
     (str (:type/name src)
          " -> "
          (:type/name dst)
          " "
          "["
          (edge-label-name (append-cardinality-label name cardinality))
          (if extra extra "")
          "]\n")
     "")))

(defn ^:private parse-param-edge
  [{:keys [param/name param/cardinality] :as param}]
  (let [src (-> param :param/parent :field/parent)
        dst (-> param :param/type)
        n (-> param :param/parent :field/name)]
    (edge-label n src dst cardinality "style=dotted;")))

(defn ^:private parse-params-edges
  [params]
  (reduce (fn [c param]
            (str c (parse-param-edge param)))
          "" params))

(defn ^:private parse-field-edge
  [{:keys [field/name field/cardinality param/_parent] :as field}]
  (let [src (-> field :field/parent)
        dst (-> field :field/type)]
    (str (edge-label name src dst cardinality)
         (parse-params-edges _parent))))

(defn ^:private parse-implement-edge
  [src dst]
  (edge-label nil src dst nil "style=dashed;arrowhead=empty;"))

(defn ^:private parse-implements-edges
  [{:keys [type/implements] :as t}]
  (reduce (fn [a impl]
            (str a (parse-implement-edge t impl)))
          "" implements))

(defn ^:private parse-union-edge
  [t field]
  (let [dst {:type/name (:field/name field)
             :type/nature :user}]
    (edge-label nil t dst nil "style=dashed;")))

(defn ^:private parse-union-edges
  [{:keys [type/union field/_parent] :as t}]
  (if union
    (reduce (fn [a field]
              (str a (parse-union-edge t field)))
            "" _parent)
    ""))

(defn ^:private parse-edge
  [{:keys [field/_parent] :as t}]
  (reduce (fn [a field]
            (str a (parse-field-edge field)))
          "" _parent))

(defn ^:private parse-edges
  [types]
  (reduce (fn [c t]
            (str c
                 (parse-edge t)
                 (parse-implements-edges t)
                 (parse-union-edges t)))
          "" types))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn ^:private meta-query
  [groups tags]
  (-> '[:find [(pull ?e [{:type/implements [*]}
                         {:field/_parent
                          [{:field/type [*]}
                           {:field/parent [*]}
                           {:param/_parent
                            [{:param/type [*]}
                             {:param/parent
                              [{:field/parent [*]} *]}
                             *]}
                           *]}
                         *]) ...]
        :where]
      (concat '[[?e :type/name]
                [?e :type/nature :user]
                [?e :graphviz/tag true]])
      (concat (map (fn [g] ['?e :graphviz/group g]) groups))
      (concat (map (fn [t] ['?e (keyword (name t) "tag") true]) tags))
      vec))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Public functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn schema
  ([conn]
   (schema conn nil))
  ([conn {:keys [groups tags dpi]}]
   (let [types (-> (meta-query groups tags)
                   (d/q @conn))]
     (str (string/replace header "$DPI" (str (or dpi 300)))
          (parse-nodes types)
          (parse-edges types)
          footer))))

(comment
  (require '[hodur-engine.core :as engine])

  (let [conn (engine/init-schema '[^{:graphviz/tag true}
                                   default
                                   
                                   A
                                   [^{:optional true} f1
                                    ^{:type String
                                      :optional true} f2
                                    f3 [af1
                                        ^C af2
                                        ^{:type B
                                          :cardinality n
                                          :optional true}
                                        af3]]

                                   ^{:graphviz/color "navajowhite1"
                                     :ble/tag-recursive true}
                                   B
                                   [^A f1]

                                   ^{:implements LaInterface}
                                   C
                                   []

                                   ^{:interface true}
                                   LaInterface
                                   []

                                   ^{:union true
                                     :graphviz/group "group1"}
                                   Lunion
                                   [D E]

                                   ^{:graphviz/group "group1"}
                                   D
                                   []

                                   ^{:bla/tag true}
                                   E
                                   []

                                   ^{:enum true}
                                   Lenum
                                   [is-this
                                    is-that]])]
    (println (schema conn {:tags [:ble]}))))
