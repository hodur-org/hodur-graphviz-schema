(ns core-test
  (:require [clojure.java.io :as io]
            [clojure.string :as s]
            [clojure.test :refer :all]
            [hodur-engine.core :as engine]
            [hodur-graphviz-schema.core :as graphviz]))

(defn ^:private win-cr-fix [x]
  (s/replace x #"\r\n" "\n"))

(def basic-schema '[^{:graphviz/tag-recursive true}
                    Person
                    [^String first-name
                     ^String last-name]])

(def color-schema '[^{:graphviz/tag-recursive true
                      :graphviz/color "navajowhite1"}
                    Person
                    [^String name]

                    ^{:graphviz/tag-recursive true
                      :graphviz/color "lightsalmon"}
                    Business
                    [^String total-revenue]])

(def stereotype-schema '[^{:graphviz/tag-recursive true
                           :graphviz/stereotype "fact"}
                         PersonEvent
                         [^String name]

                         ^{:graphviz/tag-recursive true
                           :graphviz/stereotype "command"}
                         RetractionEvent
                         [^String id]

                         ^{:interface true
                           :graphviz/tag-recursive true}
                         Node
                         [^String id]

                         ^{:enum true
                           :graphviz/tag-recursive true}
                         Gender
                         [YES NO]

                         ^{:union true
                           :graphviz/tag-recursive true}
                         GenderNode
                         [Gender Node]])

(def grouping-schema '[^{:graphviz/tag-recursive true
                         :graphviz/group "people"
                         :foo/tag true
                         :bar/tag true}
                       Person
                       [^String first-name
                        ^String last-name]

                       ^{:graphviz/tag-recursive true
                         :graphviz/group "business"
                         :foo/tag true}
                       BusinessUnit
                       [^String name]])

(deftest test-basic
  (let [s-target (-> "basic.dot" io/resource slurp win-cr-fix)
        s-mine (-> basic-schema engine/init-schema graphviz/schema)]
    (is (= s-target s-mine))))

(deftest test-color
  (let [s-target (-> "color.dot" io/resource slurp win-cr-fix)
        s-mine (-> color-schema engine/init-schema graphviz/schema)]
    (is (= s-target s-mine))))

(deftest test-stereotype
  (let [s-target (-> "stereotype.dot" io/resource slurp win-cr-fix)
        s-mine (-> stereotype-schema engine/init-schema graphviz/schema)]
    (is (= s-target s-mine))))

(deftest test-dpi
  (let [s-target (-> "basic-150.dot" io/resource slurp win-cr-fix)
        s-mine (-> basic-schema engine/init-schema (graphviz/schema {:dpi 150}))]
    (is (= s-target s-mine)))
  (let [s-target (-> "basic-600.dot" io/resource slurp win-cr-fix)
        s-mine (-> basic-schema engine/init-schema (graphviz/schema {:dpi 600}))]
    (is (= s-target s-mine))))

(deftest test-grouping
  (are [dot filter-map]
      (let [s-target (-> dot io/resource slurp win-cr-fix)
            s-mine (-> grouping-schema
                       engine/init-schema
                       (graphviz/schema filter-map))]
        (= s-target s-mine))
    
    "grouping-all.dot"
    {:groups ["people" "business"]}

    "grouping-all.dot"
    {:tags [:foo]}

    "grouping-all.dot"
    {:groups ["business"] :tags [:bar]}

    "grouping-all.dot"
    {:tags [:foo :bar]}

    "grouping-all.dot"
    {:groups ["business"] :tags [:foo]}

    "grouping-people.dot"
    {:tags [:bar]}

    "grouping-people.dot"
    {:tags [:bar] :groups ["people"]}

    "grouping-people.dot"
    {:groups ["people"]}

    "grouping-business.dot"
    {:groups ["business"]}))
