(ns datacore.ui.view.table
  (:require [datacore.ui.view :as view]
            [datacore.ui.util :refer [with-status-line callback]]
            [datacore.ui.java-fx :as fx]
            [datacore.ui.interactive :as in :refer [defin]]
            [datacore.cells :as c]
            [datacore.ui.observable :refer [observable-list]])
  (:import [javafx.util Callback]
           [javafx.beans.property ReadOnlyObjectWrapper]
           [java.util Date]
           [javafx.scene.control SelectionMode ControlUtils]))

(defn selected-cells [table]
  (into []
        (for [cell (-> table .getSelectionModel .getSelectedCells)]
          {:column (.getColumn cell)
           :row    (.getRow cell)})))

(defin scroll-to-top
  {:alias :table/scroll-to-top
   :params [[:component ::in/main-component]]}
  [{:keys [component]}]
  (.scrollTo component (-> component .getItems first))
  (doto (-> component .getSelectionModel)
    (.clearSelection)
    (.selectFirst)))

(defin scroll-to-bottom
  {:alias :table/scroll-to-bottom
   :params [[:component ::in/main-component]]}
  [{:keys [component]}]
  (.scrollTo component (-> component .getItems last))
  (doto (-> component .getSelectionModel)
    (.clearSelection)
    (.selectLast)))

(defin scroll-to-first-column
  {:alias :table/scroll-to-first-column
   :params [[:component ::in/main-component]]}
  [{:keys [component]}]
  (.scrollToColumnIndex component 0)
  (when-let [selected-row (some-> component selected-cells first :row)]
    (let [column (-> component .getColumns first)]
      (-> component .getSelectionModel (.clearAndSelect selected-row column)))))

(defin scroll-to-last-column
  {:alias :table/scroll-to-last-column
   :params [[:component ::in/main-component]]}
  [{:keys [component]}]
  (let [last-index (-> component .getColumns count dec)]
    (.scrollToColumnIndex component last-index)
    (when-let [selected-row (some-> component selected-cells first :row)]
      (let [column (-> component .getColumns last)]
        (-> component .getSelectionModel (.clearAndSelect selected-row column))))))

(defn column [name cell-value-fn]
  (fx/make
   :scene.control/table-column
   {:fx/args [name]
    :fx/setup
    #(.setCellValueFactory % (callback (fn [x] (ReadOnlyObjectWrapper. (cell-value-fn (.getValue x))))))}))

#_(defmethod view/build-view :datacore.view/table
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

(defmethod view/build-view :datacore.ui.view/table
  [view-cell]
  (let [table     (fx/make :scene.control/table-view
                           {:fx/setup
                            (fn [table]
                              (fx/set-field-in! table [:selection-model :selection-mode] SelectionMode/MULTIPLE)
                              (fx/set-field! table :style-class ["table-view" "main-component"])
                              (fx/set-field-in! table [:selection-model :cell-selection-enabled] true)
                              (-> table
                                  .getSelectionModel
                                  .getSelectedItems
                                  (.addListener
                                   (fx/list-change-listener
                                    (fn [selected]
                                      (println "TABLE SELECTION: " (pr-str selected)))))))})
        set-data! (fn [{:keys [columns column-labels data]}]
                    (fx/run-later!
                     #(doto table
                        (fx/set-field!
                         :columns (map (fn [c]
                                         (column (if-let [l (get column-labels c)] l (str c))
                                                 (fn [row] (get row c))))
                                       columns))
                        (fx/set-field! :items (observable-list data)))))]
    (set-data! (c/value view-cell))
    (c/add-watch! view-cell :table-view (fn [_ _ new] (set-data! new)))
    (with-status-line
      table (c/formula #(str (:label %) " - "
                             (-> % :data count) " rows - "
                             (-> % :columns count) " columns - "
                             (Date. (:last-modified %)))
                       view-cell
                       {:label :table-status-line}))))
