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
  (let [chart        (fx/make-tree
                      {:fx/type     :scene.chart/bar-chart
                       :fx/args     [{:fx/type :scene.chart/category-axis
                                      :label   "Values"}
                                     {:fx/type :scene.chart/number-axis
                                      :label   "Frequencies"}]
                       :style-class ["chart" "bar-chart" "main-component"]
                       :animated    false
                       :data        (observable-list [(xy-data-series "Count" (c/value view-cell))])})
        view         (view/configure-view
                      {:cell      view-cell
                       :focused?  false
                       :component (with-status-line chart "chart!")})
        watcher-name (gensym :chart-view)
        update-data! (fn [input]
                      (fx/run-later!
                       #(fx/set-field! chart :data (observable-list [(xy-data-series "Count" input)]))))
        add-watch!   (fn []
                       (c/add-watch! view-cell watcher-name
                                     (fn [_ _ new]
                                       (update-data! new))))]
    (add-watch!)
    (fx/set-field!
     view
     :fx/prop-listener [:visible (fn [source observable old visible] ;;this only works because datacore.ui.windows/replace! sets the visibility to false
                                   (prn '--watcher-removed watcher-name)
                                   (if visible
                                     (do
                                       (update-data! (c/value view-cell))
                                       (add-watch!))
                                     (c/remove-watch! view-cell watcher-name)))])

    view))

;;(def cc (c/cell nil))
;;(datacore.ui.util/inspect cc)
;;(c/reset! cc (frequency-chart))


;; interactive

(defin derive-frequency-chart
  {:alias :chart/frequencies
   :params [[:upstream-cell ::in/cell]
            [:code {:type   ::in/clojure-code
                    :title  "Derive frequencies chart"
                    :prompt "Enter a Clojure expression to run frequencies on"}]]}
  [{:keys [upstream-cell code]}]
  (let [actual-code `(fn [{:keys [~'data] :as ~'input}]
                       (->> ~'data (map ~(edn/read-string code)) frequencies (sort-by first)))
        chart-cell  (c/formula (eval actual-code)
                               upstream-cell
                               {:label :chart-view
                                :meta  {:roles #{:transform :view}
                                        :code  actual-code
                                        :raw-code code}})
        view        (frequency-chart chart-cell)]
    (windows/new-split-view view :right)))

;;(->> data (map :year) frequencies (sort-by first))
;;(->> data (map :title-type) frequencies (sort-by first))
;;(filter (fn [row] (>= (-> row :year Integer/parseInt) 1980)) data)
