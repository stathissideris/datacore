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
            [clojure.edn :as edn]
            [clojure.walk :as walk])
  (:import [de.jensd.fx.glyphs.fontawesome FontAwesomeIcon FontAwesomeIconView]
           [de.jensd.fx.glyphs.materialicons MaterialIcon MaterialIconView]
           [javafx.scene.paint Color]
           [javafx.scene.control Tooltip]
           [javafx.geometry Pos]))

(defn- has-role? [cells cell-id role]
  (let [roles (get-in cells [:meta cell-id])]
    (some? (get roles role))))

(defn- next-in-chain [cells cell-id]
  (first (c/sinks cells cell-id)))

(defn- cell-chain [cells start-cell]
  (->> (iterate (partial next-in-chain cells) start-cell)
       (take-while (complement nil?))))

(defn- role-icon [role roles class icon]
  (when (get roles role)
    (let [icon    (fx/make class {:fx/args [icon]})
          tooltip (Tooltip. (str/upper-case (name role)))] ;;javafx has this very weird bug that results
                                                           ;;in lowercase "transform" and "source" being blank in the tooltip
      (Tooltip/install icon tooltip)
      icon)))

(defn- role-icons [roles]
  [(role-icon :view roles FontAwesomeIconView FontAwesomeIcon/EYE)
   (role-icon :system roles FontAwesomeIconView FontAwesomeIcon/CIRCLE_THIN)
   (role-icon :control roles FontAwesomeIconView FontAwesomeIcon/USER_ALT)
   (role-icon :source roles MaterialIconView MaterialIcon/INPUT)
   (role-icon :transform roles MaterialIconView MaterialIcon/TRANSFORM)])

(defn- cell-box [cells cell [x y]]
  (let [height 45
        roles  (-> cells :meta (get cell) :roles)]
   {:fx/type :scene/group
    :children
    [{:fx/type     :scene.layout/h-box
      :alignment   Pos/CENTER
      :translate-x x
      :translate-y y
      :pref-width  150
      :pref-height height
      :style       "-fx-background-color: lightgrey;"
      :children
      (concat
       (role-icons roles)
       [{:fx/type :scene.control/label
         :text    (-> cells :cells (get cell) :label name)}])}]}))

(defn- source-chain [cells start-cell]
  (for [[idx cell] (map-indexed vector (cell-chain cells start-cell))]
    (cell-box cells cell [0 (* 75 idx)])))

(defn- cells-graph-elements [cells]
  (def cc cells)
  (let [sources (->> cells :meta (filter (fn [[id meta]] (get (:roles meta) :source))) (map first))
        ;; (->> cells :cells
        ;;      (filter (fn [[id meta]] (has-role? cells id :source)))
        ;;      (map first))
        ]
    (apply concat (for [source sources] (source-chain cells source)))))

(defn cells-graph
  [cells]
  (view/configure-view
   {:focused? true
    :component
    (with-status-line
      (fx/make-tree
       {:fx/type     :scene.control/scroll-pane
        :style-class ["scroll-pane" "main-component"]
        :content     {:fx/type  :scene/group
                      :children (cells-graph-elements @cells)}})
      "cells!")}))

(defn- roles-cell [roles]
  (let [remaining-roles (disj roles :view :system :control :source :transform)]
    (fx/make :scene.layout/h-box
             {:alignment Pos/TOP_CENTER
              :spacing 2
              :children
              (conj (role-icons roles)
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

(defn- format-code [code]
  (->> code
       (walk/postwalk
        (fn [x]
          (if (and (symbol? x) (= "clojure.core" (namespace x)))
            (symbol (name x))
            x)))
       pr-str))

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
                         (table/column "code" (fn [c]
                                                (if (:code c)
                                                  (format-code (:code c))
                                                  "")))
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
                                   :meta {:roles #{:transform}
                                          :code  code}})
        upstream       (first (c/sources cell))]
    (c/linear-insert! upstream transform-cell cell)))

;;(filter (fn [e] (= "Documentary" (:title-type e))) data)
