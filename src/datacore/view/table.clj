(ns datacore.view.table
  (:require [datacore.view :as view]
            [datacore.ui.util :refer [with-status-line]]
            [datacore.ui.java-fx :as fx]
            [datacore.ui.message :as message]
            [datacore.cells :as c]
            [datacore.ui.observable :refer [observable-list]])
  (:import [javafx.util Callback]
           [javafx.beans.property ReadOnlyObjectWrapper]))

(defn callback [fun]
  (reify Callback
    (call [this p]
      (fun p))))

(defn column [name cell-value-fn]
  (fx/make
   :scene.control/table-column
   {:fx/args [name]
    :fx/setup
    #(.setCellValueFactory % (callback (fn [x] (ReadOnlyObjectWrapper. (cell-value-fn (.getValue x))))))}))

(defmethod view/build-view :datacore.view/table
  [view-cell]
  (with-status-line
    (fx/make
     :scene.control/table-view
     {:fx/args [(observable-list
                 (c/formula (comp rest :data)
                            view-cell
                            {:label :table-view-data}))]
      :columns (c/formula (fn [source]
                            (map-indexed
                             (fn [i c]
                               (column (str c) #(nth % i)))
                             (first (:data source))))
                          view-cell
                          {:label :table-view-columns})})
    (c/formula :label view-cell {:label :table-status-line})))
