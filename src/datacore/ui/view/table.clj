(ns datacore.ui.view.table
  (:require [datacore.ui.view :as view]
            [datacore.ui.util :refer [with-status-line callback]]
            [datacore.ui.java-fx :as fx]
            [datacore.ui.interactive :as in :refer [defin]]
            [datacore.cells :as c]
            [datacore.ui.observable :refer [observable-list]]
            [datacore.ui.view.edn :as view.edn]
            [datacore.ui.windows :as windows]
            [clojure.pprint :refer [pprint]])
  (:import [javafx.util Callback]
           [javafx.beans.property ReadOnlyObjectWrapper]
           [java.util Date]
           [javafx.scene.control
            SelectionMode ControlUtils TableView TableColumnBase TableSelectionModel]))

(def scroll-fns [:table/scroll-up :table/scroll-down
                 :table/scroll-to-top :table/scroll-to-bottom
                 :table/scroll-to-first-column :table/scroll-to-last-column
                 :table/recenter])

(defmethod fx/fget [TableView :dc/cursor]
  [table _]
  (let [cell (some-> table .getSelectionModel .getSelectedCells first)]
    (when cell
      {:column       (.getTableColumn cell)
       :column-index (.getColumn cell)
       :row          (.getRow cell)})))

(defmethod fx/fset [TableView :dc/cursor]
  [table _ {:keys [row column]}]
  (let [model (some-> table .getSelectionModel)]
    (doto model
      (.clearSelection)
      (.select row (or column (-> table .getColumns first))))))

(defin scroll-to-top
  {:alias :table/scroll-to-top
   :related scroll-fns
   :params [[:component ::in/main-component]]}
  [{:keys [component]}]
  (let [first-column (-> component .getColumns first)]
    (.scrollTo component (-> component .getItems first))
    (.scrollToColumn component first-column)
    (doto (-> component .getSelectionModel)
      (.clearSelection)
      (.select 0 first-column))))

(defin scroll-to-bottom
  {:alias :table/scroll-to-bottom
   :related scroll-fns
   :params [[:component ::in/main-component]]}
  [{:keys [component]}]
  (let [last-column (-> component .getColumns last)]
    (.scrollTo component (-> component .getItems last))
    (.scrollToColumn component last-column)
    (doto (-> component .getSelectionModel)
      (.clearSelection)
      (.select (-> component .getItems .size dec) last-column))))

(defin scroll-to-first-column
  {:alias :table/scroll-to-first-column
   :related scroll-fns
   :params [[:component ::in/main-component]]}
  [{:keys [component]}]
  (.scrollToColumnIndex component 0)
  (when-let [row (:row (fx/get-field component :dc/cursor))]
    (let [column (-> component .getColumns first)]
      (-> component .getSelectionModel (.clearAndSelect row column)))))

(defin scroll-to-last-column
  {:alias  :table/scroll-to-last-column
   :related scroll-fns
   :params  [[:component ::in/main-component]]}
  [{:keys [component]}]
  (let [last-index (-> component .getColumns count dec)]
    (.scrollToColumnIndex component last-index)
    (when-let [row (:row (fx/get-field component :dc/cursor))]
      (let [column (-> component .getColumns last)]
        (-> component .getSelectionModel (.clearAndSelect row column))))))

(def ^:private scroll-to* (-> TableView (.getMethod "scrollTo" (into-array [Integer/TYPE]))))

(defn- scroll-to [table index]
  (fx/run-later!
   #(.invoke scroll-to* table (object-array [(int index)]))))

(defin scroll-up
  {:alias   :table/scroll-up
   :help    "Scroll table up"
   :related scroll-fns
   :params  [[:component ::in/main-component]]}
  [{:keys [component]}]
  (let [[first-row last-row] (fx/get-field component :fx/visible-range)]
    (when (and first-row last-row)
      (let [column (:column (fx/get-field component :dc/cursor))
            row    (max 0 (- first-row (- last-row first-row)))]
        (scroll-to component row)
        (fx/set-field! component :dc/cursor {:column column :row row})))))

(defin scroll-down
  {:alias   :table/scroll-down
   :help    "Scroll table down"
   :related scroll-fns
   :params  [[:component ::in/main-component]]}
  [{:keys [component]}]
  (let [[_ last-row] (fx/get-field component :fx/visible-range)]
    (when last-row
      (let [column       (:column (fx/get-field component :dc/cursor))]
        (scroll-to component last-row)
        (fx/set-field! component :dc/cursor {:column column :row last-row})))))

(defin recenter
  {:alias   :table/recenter
   :related scroll-fns
   :params  [[:table ::in/main-component]]}
  [{:keys [table]}]
  (let [[first-row last-row] (fx/get-field table :fx/visible-range)
        cursor-row           (:row (fx/get-field table :dc/cursor))
        center-row           (+ first-row (/ (- last-row first-row) 2))]
    (scroll-to table (inc (- first-row (- center-row cursor-row))))))

(defmethod in/resolve-param ::in/table-cursor
  [_]
  (let [table (in/resolve-param {:type ::in/main-component})]
    (fx/get-field table :dc/cursor)))
(defmethod in/resolve-param-help ::in/table-cursor
  [_] "The position of the cursor in the currently focused table.")

(defin copy-row-as-edn
  {:alias  :table/copy-row-as-edn
   :help   [:span "Put a pretty-printed "
            [:a {:href "https://github.com/edn-format/edn"} "EDN"]
            " representation of the row under the cursor into the system clipboard."]
   :params [[:data   ::in/cell-data]
            [:cursor ::in/table-cursor]]}
  [{:keys [data cursor]}]
  (let [edn (with-out-str (some-> data :data (nth (:row cursor)) pprint))]
    (fx/run-later! #(fx/to-clipboard edn))))

(defin row-edn-view
  {:alias :table/row-edn-view
   :params [[:component ::in/focus-parent]]}
  [{:keys [component]}]
  (let [table-cell (fx/get-field component :dc/cell)
        edn-cell   (c/formula (fn [{:keys [data control]}]
                                {::view/type ::view/edn
                                 :data       (when data (some->> control :selected-cells first :row (nth data)))})
                              table-cell
                              {:label :edn-view
                               :meta  {:roles #{:transform}}})
        view       (view/build-cell-view edn-cell)]
    (fx/run-later!
     (fn []
       ;;(windows/split-right)
       ;;(windows/focus-right)
       (windows/replace-focused! view)
       ;;(windows/focus-left)
       ))))

(defn column [name cell-value-fn]
  (fx/make
   :scene.control/table-column
   {:fx/args [name]
    :fx/setup
    #(.setCellValueFactory % (callback (fn [x] (ReadOnlyObjectWrapper. (cell-value-fn (.getValue x))))))}))

(defmethod fx/fget [TableView :dc/selected-cells]
  [table _]
  (for [cell (-> table .getSelectionModel .getSelectedCells)]
    {:row    (.getRow cell)
     :column (.getColumn cell)}))

(defmethod fx/fset [TableView :dc/selected-cells]
  [table _ cells]
  (let [model (.getSelectionModel table)]
    (.clearSelection model)
    (doseq [{:keys [row column]} cells]
      (.select model row (nth (.getColumns table) column)))))

(defn retain-selection [table fun]
  (let [cells (fx/get-field table :dc/selected-cells)]
    (fun)
    (fx/set-field! table :dc/selected-cells cells)))

(defn retain-focus [table fun]
  (let [focus (-> table .getFocusModel .getFocusedCell)]
    (fun)
    (-> table .getFocusModel .focus focus)))

(defmethod view/got-focus TableView
  [this]
  (when (empty? (fx/get-field this :dc/selected-cells))
    (fx/run-later!
     #(fx/set-field! this :dc/selected-cells [{:row 0 :column 0}]))))

(defmethod view/build-cell-view ::view/table
  [view-cell]
  (let [control-cell (c/cell :table-control {})]
    (c/link-slot! control-cell view-cell 1)
    (c/alter-meta! control-cell assoc :roles #{:control})
    @(fx/run-later!
      (fn []
        (let [data-cell       (c/formula (fn [x]
                                           (prn '>>>table-data-cell)
                                           (:data x)) view-cell {:label :table-data
                                                                 :meta  {:roles #{:system}}})
              ;;stopgap cell to prevent columns-cell for updating when
              ;;there is a change in data only
              columns-stopgap (c/formula #(select-keys % [:columns :column-labels])
                                         view-cell
                                         {:meta {:roles #{:system}}})
              columns-cell    (c/formula
                               (fn [{:keys [columns column-labels]}]
                                 (prn '>>>table-columns-cell columns column-labels)
                                 (map (fn [c]
                                        (column (if-let [l (get column-labels c)] l (str c))
                                                (fn [row] (get row c))))
                                      columns))
                               columns-stopgap
                               {:meta {:roles #{:system}}})]
          (view/configure-view
           {:cell      view-cell
            :focused?  true
            :component
            (-> (fx/make-tree
                 {:fx/type                  :scene.control/table-view
                  :style-class              ["table-view" "main-component"]
                  :items                    (observable-list data-cell)
                  :columns                  columns-cell
                  [:selection-model
                   :selection-mode]         SelectionMode/MULTIPLE
                  [:selection-model
                   :cell-selection-enabled] true
                  :fx/setup
                  (fn [table]
                    (-> table
                        .getSelectionModel
                        .getSelectedCells
                        (.addListener
                         (fx/list-change-listener
                          (fn [selected-cells]
                            (c/hidden-swap! control-cell #{view-cell data-cell columns-cell} assoc :selected-cells
                                            (for [cell selected-cells]
                                              {:row    (.getRow cell)
                                               :column (.getColumn cell)})))))))})
                (with-status-line
                  (c/formula #(str (:label %) " - "
                                   (-> % :data count) " rows - "
                                   (-> % :columns count) " columns - "
                                   (Date. (:last-modified %))
                                   " | select: " (or (some-> % :selection-mode name (str "s")) "cells"))
                             view-cell
                             {:label :table-status-line
                              :meta  {:roles #{:system}}})))}))))))
