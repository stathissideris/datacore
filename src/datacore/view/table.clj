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
  [{:keys [label source transformers] :as view}]
  (if-let [data-fn (:data-fn source)]
    (let [original-data (data-fn)
          data          (view/transform original-data transformers)]
      #_(with-status-line
        (fx/make
         :scene.control/table-view
         {:fx/args [(observable-list (view/transform (rest data) transformers))]
          :columns (map-indexed (fn [i c] (column (str c) #(nth % i))) (first data))})
        (c/cell= (str label (when-not (empty? transformers)
                                (str " - " (count transformers) " transformers")))))
      (fx/make
       :scene.control/table-view
       {:fx/args [(observable-list (view/transform (rest data) transformers))]
        :columns (map-indexed (fn [i c] (column (str c) #(nth % i))) (first data))}))
    (do
      (message/error "nil data-fn function")
      nil)))
