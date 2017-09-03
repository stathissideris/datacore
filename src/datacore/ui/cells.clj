(ns datacore.ui.cells
  (:require [datacore.ui.java-fx :as fx]
            [datacore.ui.view :as view]
            [datacore.ui.interactive :refer [defin] :as in]
            [datacore.ui.windows :as windows]
            [datacore.cells :as c]
            [datacore.util :as util]
            [datacore.ui.util :refer [with-status-line]]
            [datacore.ui.view.table :as table]
            [datacore.ui.observable :refer [observable-list]]
            [clojure.string :as str]
            [clojure.edn :as edn])
  (:import [de.jensd.fx.glyphs.fontawesome FontAwesomeIcon FontAwesomeIconView]
           [de.jensd.fx.glyphs.materialicons MaterialIcon MaterialIconView]
           [javafx.scene.paint Color]
           [javafx.scene.control Tooltip]
           [javafx.geometry Pos]))

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
          tooltip (Tooltip. (str/upper-case (name role)))] ;;javafx has this very weird bug that results
                                                           ;;in lowercase "transform" and "source" being blank in the tooltip
      (Tooltip/install icon tooltip)
      icon)))

(defn- roles-cell [roles]
  (let [remaining-roles (disj roles :view :system :control :source :transform)]
    (fx/make :scene.layout/h-box
             {:alignment Pos/TOP_CENTER
              :spacing 2
              :children
              (remove nil?
                      [(role-icon :view roles FontAwesomeIconView FontAwesomeIcon/EYE)
                       (role-icon :system roles FontAwesomeIconView FontAwesomeIcon/CIRCLE_THIN)
                       (role-icon :control roles FontAwesomeIconView FontAwesomeIcon/USER_ALT)
                       (role-icon :source roles MaterialIconView MaterialIcon/INPUT)
                       (role-icon :transform roles MaterialIconView MaterialIcon/TRANSFORM)
                       (when-not (empty? remaining-roles)
                         (fx/make :scene.control/label {:text (pr-str remaining-roles)}))])})))

(defn- boolean-cell [bool]
  (if-not bool
    (fx/make :scene.control/label {:text ""})
    (fx/make MaterialIconView {:fx/args [MaterialIcon/CHECK]})))

(defn- index-of-cell [items cell-id]
  (->> (map-indexed vector items)
       (filter (comp #(= cell-id (:id %)) second))
       ffirst))

(defn- links-cell [table links]
  (if (empty? links)
    (fx/make :scene.control/label {:text ""})
    (fx/make :scene.layout/flow-pane
             {:children
              (for [link links]
                (fx/make :scene.control/hyperlink
                         {:text (str link)
                          :text-fill (Color/web "0x0000A0")
                          :on-mouse-clicked (fx/event-handler
                                             (fn [e]
                                               (when-let [row (index-of-cell (.getItems table) link)] ;;might be filtered
                                                 (fx/run-later!
                                                  #(fx/set-field! table :dc/cursor {:row (int row)})))))}))})))

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
    (util/add-meta! table {:show-system? false
                           :refresh-fn   (fn [] (swap! table-cells-atom identity))}) ;;trigger watch
    (add-watch cells-atom watcher-name
               (fn [_ _ old new]
                 (let [show-system? (-> table util/meta :show-system?)]
                   (->> (c/all-cells)
                        (filter #(or show-system?
                                     (-> % :meta :roles :system not)))
                        (map #(update % :value
                                      (fn [x] (if (instance? javafx.scene.Node x)
                                                "JavaFX component"
                                                (-> x str (util/truncate-string 100))))))
                        (reset! table-cells-atom)))))
    (fx/set-field!
     component
     :fx/prop-listener [:visible (fn [source observable old visible] ;;this only works because datacore.ui.windows/replace! sets the visibility to false
                                   (if-not visible
                                     (remove-watch cells-atom watcher-name)))])
    (fx/set-fields!
     table
     {:items            (observable-list table-cells-atom)
      :columns          [(table/column "id" :id)
                         (table/column
                          "roles" :roles
                          (fx/callback
                           (fn [_]
                             (doto (table/cell
                                    {:update-item
                                     (fn [cell roles empty?]
                                       (when (and (not empty?) (not (clojure.core/empty? roles)))
                                         (.setGraphic cell (roles-cell roles))))})
                               (.setAlignment Pos/TOP_CENTER)))))
                         (table/column "label" :label)
                         (table/column
                          "input?" :formula?
                          (fx/callback
                           (fn [_]
                             (let [checkmark (fx/make MaterialIconView {:fx/args [MaterialIcon/CHECK]})]
                               (doto (table/cell
                                      {:update-item
                                       (fn [cell formula? empty?]
                                         (when (not (or formula? empty?))
                                           (.setGraphic cell checkmark)))})
                                 (.setAlignment Pos/TOP_CENTER))))))
                         (table/column
                          "enabled?" :enabled?
                          (fx/callback
                           (fn [_]
                             (let [checkmark (fx/make MaterialIconView {:fx/args [MaterialIcon/CHECK]})]
                               (doto (table/cell
                                      {:update-item
                                       (fn [cell enabled? empty?]
                                         (when enabled?
                                           (.setGraphic cell checkmark)))})
                                 (.setAlignment Pos/TOP_CENTER))))))
                         (table/column "value" :value)
                         (table/column "error" :error)
                         (table/column
                          "sinks" :sinks
                          (fx/callback
                           (fn [_]
                             (table/cell
                              {:update-item
                               (fn [cell links empty?]
                                 (when-not empty?
                                   (.setGraphic cell (links-cell table (sort links)))))}))))
                         (table/column
                          "sources" :sources
                          (fx/callback
                           (fn [_]
                             (table/cell
                              {:update-item
                               (fn [cell links empty?]
                                 (when-not empty?
                                   (.setGraphic cell (links-cell table links))))}))))
                         (table/column "meta" :meta)]})
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
   :params [[:cursor ::in/table-cursor]
            [:table ::in/main-component]]}
  [{:keys [cursor table]}]
  (when (and cursor table)
    (let [component (some-> (into [] (.getItems table))
                            (nth (:row cursor))
                            :cell
                            c/meta
                            :component)]
      (when component
        (if (some? (first (filter #(= % component) (fx/tree-seq fx/top-level))))
          (fx/run-later! #(view/focus! component)) ;;TODO bring window to front?
          (fx/run-later! #(windows/replace-focused! component)))))))

(defin toggle-mute
  {:alias :cells/toggle-mute
   :params [[:cursor ::in/table-cursor]
            [:table ::in/main-component]]}
  [{:keys [cursor table]}]
  (when (and cursor table)
    (let [cell (some-> (into [] (.getItems table))
                       (nth (:row cursor))
                       :cell)]
      (when cell (c/toggle-mute! cell))
      (fx/run-later!
       #(fx/set-field! table :dc/cursor {:row (:row cursor)})))))

(defin toggle-system
  {:alias :cells/toggle-system
   :params [[:component ::in/main-component]]}
  [{:keys [component]}]
  (util/alter-meta! component update :show-system? not)
  ((-> component util/meta :refresh-fn)))

(defin add-transform-cell
  {:alias :cells/add-transform-cell
   :params [[:cell ::in/cell]
            [:code {:type   ::in/clojure-code
                    :title  "Transform cell code"
                    :prompt "Enter a Clojure expression"}]]}
  [{:keys [cell code]}]
  (let [code           `(fn [{:keys [~'data] :as ~'input}]
                          (assoc ~'input :data ~(edn/read-string code)))
        transform-cell (c/formula (eval code) ::c/unlinked
                                  {:label :transform-cell
                                   :meta {:roles #{:transform}}})
        upstream       (first (c/sources cell))]
    (c/linear-insert! upstream transform-cell cell)))

;;(filter (fn [e] (= "Documentary" (:title-type e))) data)
