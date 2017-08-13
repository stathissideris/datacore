(ns datacore.ui.cells
  (:require [datacore.ui.java-fx :as fx]
            [datacore.ui.view :as view]
            [datacore.ui.interactive :refer [defin]]
            [datacore.ui.windows :as windows]
            [datacore.cells :as c]
            [datacore.util :as util]
            [datacore.ui.util :refer [with-status-line]]
            [datacore.ui.view.table :as table]
            [datacore.ui.observable :refer [observable-list]]))

(defn cell-graph-elements [cells-atom]
  [{:fx/type :scene.shape/rectangle
    :x       50
    :y       50
    :width   300
    :height  200
    :fill    (fx/color "red")}
   {:fx/type :scene.shape/rectangle
    :x       150
    :y       150
    :width   300
    :height  200
    :fill    (fx/color "blue")}])

(defn cells-graph
  [cells-atom]
  (view/build-view
   {::view/type ::view/default
    :focused? true
    :view
    (with-status-line
      (fx/make-tree
       {:fx/type     :scene.control/scroll-pane
        :style-class ["scroll-pane" "main-component"]
        :content     {:fx/type  :scene/group
                      :children (cell-graph-elements cells-atom)}})
      "cells!")}))



(defn cells-table
  [cells-atom]
  (let [table            (fx/make-tree
                          {:fx/type  :scene.control/table-view
                           :fx/setup
                           (fn [table]
                             ;;(fx/set-field-in! table [:selection-model :selection-mode] SelectionMode/MULTIPLE)
                             (fx/set-field! table :style-class ["table-view" "main-component"])
                             (fx/set-field-in! table [:selection-model :cell-selection-enabled] true))})
        table-cells-atom (atom [])]
    (fx/set-field!
     table
     :columns (map (fn [c]
                     (table/column (str c)
                                   (fn [row] (get row c))))
                   [:id :roles :label :formula? :enabled? :value :error :sinks :sources :meta]))
    (-> table .getColumns (nth 4) (.setPrefWidth 200))
    (fx/set-field! table :items (observable-list table-cells-atom))
    (add-watch cells-atom (gensym :table-view)
               (fn [_ _ old new]
                 (reset! table-cells-atom
                         (map #(update % :value
                                       (fn [x] (if (instance? javafx.scene.Node x)
                                                 "JavaFX component"
                                                 (-> x str (util/truncate-string 100)))))
                              (c/all-cells)))))
    (view/build-view
     {::view/type ::view/default
      :focused? true
      :view
      (with-status-line
        table
        "cells!")})))

;; interactive

(defin show-graph
  {:alias :cells/show-graph}
  []
  (let [component (cells-graph @#'c/global-cells)]
    (fx/run-later!
     #(windows/replace-focused! component))))

(defin show-table
  {:alias :cells/show-table}
  []
  (let [component (cells-table @#'c/global-cells)]
    (fx/run-later!
     #(windows/replace-focused! component))))
