(ns core-test
  (:require [clojure.java.io :as io]
            [clojure.test :refer :all]
            [hodur-graphviz-schema.core :as graphviz]
            [hodur-engine.core :as engine]))

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

(deftest test-basic
  (let [s-target (-> "basic.dot" io/resource slurp)
        s-mine (-> basic-schema engine/init-schema graphviz/schema)]
    (is (= s-target s-mine))))

(deftest test-color
  (let [s-target (-> "color.dot" io/resource slurp)
        s-mine (-> color-schema engine/init-schema graphviz/schema)]
    (is (= s-target s-mine))))

(deftest test-dpi
  (let [s-target (-> "basic-150.dot" io/resource slurp)
        s-mine (-> basic-schema engine/init-schema (graphviz/schema {:dpi 150}))]
    (is (= s-target s-mine)))
  (let [s-target (-> "basic-600.dot" io/resource slurp)
        s-mine (-> basic-schema engine/init-schema (graphviz/schema {:dpi 600}))]
    (is (= s-target s-mine))))
