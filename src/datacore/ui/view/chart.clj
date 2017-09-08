(ns datacore.ui.view.chart
  (:require [datacore.ui.java-fx :as fx]
            [datacore.ui.observable :refer [observable-list]]
            [datacore.cells :as c])
  (:import [javafx.scene.chart XYChart]))

(defn xy-data-series
  ([entries]
   (xy-data-series entries))
  ([series-name entries]
   (fx/make-tree
    {:fx/type javafx.scene.chart.XYChart$Series
     :name series-name
     :data
     (observable-list
      (for [[x y] entries]
        (fx/make-tree
         {:fx/type javafx.scene.chart.XYChart$Data :fx/args [x y]})))})))

(defn frequency-chart []
  (fx/make-tree
   {:fx/type :scene.chart/bar-chart
    :fx/args [{:fx/type :scene.chart/category-axis
               :label   "Values"}
              {:fx/type :scene.chart/number-axis
               :label   "Frequencies"}]
    :data
    (observable-list
     [(xy-data-series
       "Frequencies"
       [["foo" 100]
        ["bar" 63]
        ["baz" 110]])])}))

;;(def cc (c/cell nil))
;;(datacore.ui.util/inspect cc)
;;(c/reset! cc (frequency-chart))
