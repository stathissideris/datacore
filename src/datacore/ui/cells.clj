(ns datacore.ui.cells
  (:require [datacore.ui.java-fx :as fx]
            [datacore.ui.view :as view]
            [datacore.ui.interactive :refer [defin] :as in]
            [datacore.ui.windows :as windows]
            [datacore.cells :as c]
            [datacore.util :as util]
            [datacore.ui.util :refer [with-status-line]]
            [datacore.ui.view.table :as table]
            [datacore.ui.observable :refer [observable-list]])
  (:import [de.jensd.fx.glyphs.fontawesome FontAwesomeIcon FontAwesomeIconView]
           [de.jensd.fx.glyphs.materialicons MaterialIcon MaterialIconView]
           [javafx.scene.paint Color]
           [javafx.scene.control Tooltip]))

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
  (view/configure-view
   {:focused? true
    :component
    (with-status-line
      (fx/make-tree
       {:fx/type     :scene.control/scroll-pane
        :style-class ["scroll-pane" "main-component"]
        :content     {:fx/type  :scene/group
                      :children (cell-graph-elements cells-atom)}})
      "cells!")}))

(defn- role-icon [role roles class icon]
  (when (get roles role)
    (let [icon    (fx/make class {:fx/args [icon]})
          tooltip (Tooltip. (name role))]
      (Tooltip/install icon tooltip)
      icon)))

(defn- roles-cell [roles]
  (if (empty? roles)
    (fx/make :scene.control/label {:text ""})
    (let [remaining-roles (disj roles :view :system :control :source :transform)]
      (fx/make :scene.layout/h-box
               {:children
                (remove nil?
                        [(role-icon :view roles FontAwesomeIconView FontAwesomeIcon/EYE)
                         (role-icon :system roles FontAwesomeIconView FontAwesomeIcon/CIRCLE_THIN)
                         (role-icon :control roles FontAwesomeIconView FontAwesomeIcon/USER_ALT)
                         (role-icon :source roles MaterialIconView MaterialIcon/INPUT)
                         (role-icon :transform roles MaterialIconView MaterialIcon/TRANSFORM)
                         (when-not (empty? remaining-roles)
                           (fx/make :scene.control/label {:text (pr-str remaining-roles)}))])}))))

(defn- boolean-cell [bool]
  (if-not bool
    ""
    (fx/make MaterialIconView {:fx/args [MaterialIcon/CHECK]})))

(defn- links-cell [table links]
  (if (empty? links)
    ""
    (fx/make :scene.layout/flow-pane
             {:children
              (for [link links]
                (fx/make :scene.control/hyperlink
                         {:text (str link)
                          :text-fill (Color/web "0x0000A0")
                          :on-mouse-clicked (fx/event-handler
                                             (fn [e]
                                               (fx/run-later!
                                                #(fx/set-field! table :dc/cursor {:row (int link)}))))}))})))

(defn cells-table
  [cells-atom]
  (let [table            (fx/make-tree
                          {:fx/type  :scene.control/table-view
                           :fx/setup
                           (fn [table]
                             ;;(fx/set-field-in! table [:selection-model :selection-mode] SelectionMode/MULTIPLE)
                             (fx/set-field! table :style-class ["table-view" "main-component"])
                             ;;(fx/set-field-in! table [:selection-model :cell-selection-enabled] true)
                             )})
        component        (with-status-line
                           table
                           "cells!")
        table-cells-atom (atom [])
        watcher-name     (gensym :table-view)]
    (add-watch cells-atom watcher-name
               (fn [_ _ old new]
                 (reset! table-cells-atom
                         (map #(update % :value
                                       (fn [x] (if (instance? javafx.scene.Node x)
                                                 "JavaFX component"
                                                 (-> x str (util/truncate-string 100)))))
                              (c/all-cells)))))
    (fx/set-field!
     component
     :fx/prop-listener [:visible (fn [source observable old visible] ;;this only works because datacore.ui.windows/replace! sets the visibility to false
                                   (if-not visible
                                     (remove-watch cells-atom watcher-name)))])
    (fx/set-fields!
     table
     {:items            (observable-list table-cells-atom)
      :columns          (map (fn [c]
                               (table/column (name c)
                                             (fn [row]
                                               (cond (= c :roles) (roles-cell (:roles row))
                                                     (= c :input?) (boolean-cell (not (:formula? row)))
                                                     (= c :enabled?) (boolean-cell (:enabled? row))
                                                     (= c :sinks) (links-cell table (:sinks row))
                                                     (= c :sources) (links-cell table (:sources row))
                                                     :else (get row c)))))
                             [:id :roles :label :input? :enabled? :value :error :sinks :sources :meta])})
    (let [column-widths [[5 200]
                         [7 100]
                         [8 100]
                         [9 200]]]
      (doseq [[idx width] column-widths]
        (-> table .getColumns (nth idx) (.setPrefWidth width))))
    (view/configure-view
     {:focused?   true
      ::view/type ::view/cells-table
      :component  component})))

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

(defin open-view
  {:alias  :cells/open-view
   :params [[:cursor ::in/table-cursor]]}
  [{:keys [cursor]}]
  (when cursor
    (let [component (some-> (c/all-cells)
                            (nth (:row cursor))
                            :cell
                            c/meta
                            :component)]
      (fx/run-later!
       (when component
         #(windows/replace-focused! component))))))
