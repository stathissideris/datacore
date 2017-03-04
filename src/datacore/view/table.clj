(ns datacore.view.table
  (:require [datacore.view :as view]
            [datacore.ui.util :refer [with-status-line]]
            [datacore.ui.java-fx :as fx]
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
  [{:keys [source] :as view}]
  (if-not (:data-fn source)
    (prn "nil data-fn function")
    (let [data ((:data-fn source))]
      (fx/make
       :scene.control/table-view
       {:fx/args [(observable-list (rest data))]
        :columns (map-indexed (fn [i c] (column (str c) #(nth % i))) (first data))}))))
