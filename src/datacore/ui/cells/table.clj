(ns datacore.ui.cells.table
  (:require [datacore.ui.java-fx :as fx]
            [datacore.ui.cells :as ui.cells]
            [clojure.walk :as walk]
            [datacore.ui.view :as view]
            [datacore.ui.util :refer [with-status-line]]
            [datacore.util :as util]
            [datacore.cells :as c]
            [datacore.ui.observable :refer [observable-list]]
            [datacore.ui.view.table :as table]
            [datacore.ui.interactive :refer [defin] :as in]
            [datacore.ui.windows :as windows]
            [clojure.string :as str])
  (:import [javafx.geometry Pos]
           [de.jensd.fx.glyphs.materialicons MaterialIcon MaterialIconView]
           [javafx.scene.paint Color]))

(defn- roles-cell [roles]
  (let [remaining-roles (disj roles :view :system :control :source :transform)]
    (fx/make :scene.layout/h-box
             {:alignment Pos/TOP_CENTER
              :spacing 2
              :children
              (conj (ui.cells/role-icons roles)
                    (when-not (empty? remaining-roles)
                      (fx/make :scene.control/label {:text (pr-str remaining-roles)})))})))

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
  (let [table                 (fx/make-tree
                               {:fx/type  :scene.control/table-view
                                :fx/setup
                                (fn [table]
                                  ;;(fx/set-field-in! table [:selection-model :selection-mode] SelectionMode/MULTIPLE)
                                  (fx/set-field! table :style-class ["table-view" "main-component"])
                                  ;;(fx/set-field-in! table [:selection-model :cell-selection-enabled] true)
                                  )})
        component             (with-status-line
                                table
                                "cells!")
        table-cells-atom      (atom [])
        watcher-name          (gensym :table-view)
        update-table-cells-fn (fn []
                                (let [show-system? (-> table util/meta :show-system?)]
                                  (->> (c/all-cells)
                                       (filter #(or show-system?
                                                    (-> % :meta :roles :system not)))
                                       (map #(update % :value
                                                     (fn [x] (if (instance? javafx.scene.Node x)
                                                               "JavaFX component"
                                                               (-> x str (util/truncate-string 100))))))
                                       (reset! table-cells-atom))))]
    (util/add-meta! table {:show-system? false
                           :refresh-fn   update-table-cells-fn})
    (add-watch cells-atom watcher-name (fn [_ _ old new] (update-table-cells-fn)))

    (fx/set-field!
     component
     :fx/prop-listener [:visible (fn [source observable old visible] ;;this only works because datacore.ui.windows/replace! sets the visibility to false
                                   (prn '--watcher-removed watcher-name)
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
                         (table/column
                          "code" :raw-code
                          (fx/callback
                           (fn [_]
                             (table/cell
                              {:update-item
                               (fn [cell code empty?]
                                 (when-not empty?
                                   (.setText cell code)
                                   (.setStyle cell (str "-fx-font-family: \"Monaco\", monospace;"
                                                        "-fx-font-size: 0.9em;"))))}))))
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
                         [6 200]
                         [8 100]
                         [9 100]
                         [10 200]]]
      (doseq [[idx width] column-widths]
        (-> table .getColumns (nth idx) (.setPrefWidth width))))
    (view/configure-view
     {:focused?   true
      ::view/type ::view/cells-table
      :component  component})))

;; interactive

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

(defin edit-code
  {:alias :cells/edit-code
   :params [[:cursor ::in/table-cursor]
            [:table ::in/main-component]]}
  [{:keys [cursor table raw-code]}]
  (if-let [cell (some-> (into [] (.getItems table))
                        (nth (:row cursor))
                        :cell)]
    (let [old-code (-> cell c/meta :raw-code)
          new-code (in/resolve-param {:type          ::in/clojure-code
                                      :title         "Cell code"
                                      :prompt        "Enter a Clojure expression"
                                      :initial-input old-code})
          code     `(fn [{:keys [~'data] :as ~'input}]
                      (assoc ~'input :data ~(read-string new-code)))]
      (c/alter-meta! cell assoc
                     :code code
                     :raw-code new-code)
      (c/swap-function! cell (eval code)))))

(defin rename
  {:alias :cells/rename
   :params [[:cursor ::in/table-cursor]
            [:table ::in/main-component]]}
  [{:keys [cursor table raw-code]}]
  (if-let [cell      (some-> (into [] (.getItems table))
                             (nth (:row cursor))
                             :cell)]
    (let [new-label (in/resolve-param {:type          ::in/string
                                       :title         "Cell label"
                                       :prompt        "Edit the cell label"
                                       :initial-input (name (c/label cell))})]
      (c/set-label! cell (keyword (str/replace new-label " " "-"))))))

(defin edn-view
  {:alias :cells/edn-view
   :params [[:cursor ::in/table-cursor]
            [:table ::in/main-component]]}
  [{:keys [cursor table raw-code]}]
  (if-let [cell     (some-> (into [] (.getItems table))
                            (nth (:row cursor))
                            :cell)]
    (let [edn-cell (c/formula (fn [value]
                                {::view/type ::view/edn
                                 :label      (str "EDN view of cell '" (name (c/label cell)) "'")
                                 :data       value})
                              cell
                              {:label :edn-view})
          view     (view/build-cell-view edn-cell)]
      (windows/new-split-view view :right))))
