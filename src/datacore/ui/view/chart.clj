(ns datacore.ui.view.chart
  (:require [clojure.edn :as edn]
            [datacore.ui.view :as view]
            [datacore.ui.java-fx :as fx]
            [datacore.ui.windows :as windows]
            [datacore.ui.observable :refer [observable-list]]
            [datacore.ui.interactive :as in :refer [defin]]
            [datacore.ui.util :refer [with-status-line]]
            [datacore.cells :as c])
  (:import [javafx.scene.chart XYChart]))

(defn xy-data-series
  ([entries]
   (xy-data-series nil entries))
  ([series-name entries]
   (fx/make-tree
    {:fx/type javafx.scene.chart.XYChart$Series
     :name series-name
     :data
     (observable-list
      (for [[x y] entries]
        (fx/make-tree
         {:fx/type javafx.scene.chart.XYChart$Data :fx/args [x y]})))})))

(defn frequency-chart [view-cell]
  (view/configure-view
   {:cell      view-cell
    :focused?  false
    :component
    (-> (fx/make-tree
         {:fx/type     :scene.chart/bar-chart
          :fx/args     [{:fx/type :scene.chart/category-axis
                         :label   "Values"}
                        {:fx/type :scene.chart/number-axis
                         :label   "Frequencies"}]
          :style-class ["chart" "bar-chart" "main-component"]
          :data
          (observable-list
           (c/formula
            (fn [data]
              [(xy-data-series "Count" data)])
            view-cell
            {:label :chart-cell
             :meta  {:roles #{:system}}}))})
        (with-status-line "chart!"))}))

;;(def cc (c/cell nil))
;;(datacore.ui.util/inspect cc)
;;(c/reset! cc (frequency-chart))


;; interactive

(defin derive-frequency-chart
  {:alias :chart/frequencies
   :params [[:upstream-cell ::in/cell]
            [:code {:type   ::in/clojure-code
                    :title  "Frequencies chart cell code"
                    :prompt "Enter a Clojure expression"}]]}
  [{:keys [upstream-cell code]}]
  (let [code       `(fn [{:keys [~'data] :as ~'input}]
                      ~(edn/read-string code))
        chart-cell (c/formula (eval code)
                              upstream-cell
                              {:label :chart-view
                               :meta  {:roles #{:transform :view}
                                       :code  code}})
        view       (frequency-chart chart-cell)]
    (windows/new-split-view view :right)))

;;(->> data (map :year) frequencies (sort-by first))
;;(->> data (map :title-type) frequencies (sort-by first))
;;(filter (fn [row] (>= (-> row :year Integer/parseInt) 1980)) data)
