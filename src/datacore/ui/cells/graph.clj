(ns datacore.ui.cells.graph
  (:require [datacore.cells :as c]
            [datacore.ui.java-fx :as fx]
            [clojure.string :as str]
            [datacore.ui.view :as view]
            [datacore.ui.interactive :refer [defin] :as in]
            [datacore.ui.util :refer [with-status-line]]
            [datacore.ui.windows :as windows]
            [datacore.ui.cells :as ui.cells])
  (:import [de.jensd.fx.glyphs.fontawesome FontAwesomeIcon FontAwesomeIconView]
           [de.jensd.fx.glyphs.materialicons MaterialIcon MaterialIconView]
           [javafx.scene.paint Color]
           [javafx.scene.control Tooltip]
           [javafx.geometry Pos]))

(defn- has-role? [cell-id role]
  (-> cell-id c/meta :roles (get role) some?))

(defn- next-in-chain [cell-id]
  (first (c/sinks cell-id)))

(defn- cell-chain [start-cell]
  (->> (iterate next-in-chain start-cell)
       (take-while (complement nil?))))

(defn- cell-box [cell [x y]]
  (let [height 35
        roles  (:roles (c/meta cell))]
    {:fx/type :scene/group
     :style-class ["cells"]
     :children
     [{:fx/type     :scene.layout/h-box
       :alignment   Pos/CENTER
       :translate-x x
       :translate-y y
       :pref-width  150
       :pref-height height
       :style-class (if (c/muted? cell)
                      ["cell-box" "muted"]
                      ["cell-box"])
       :dc/meta     {:cell cell}
       :children
       (concat
        (ui.cells/role-icons roles)
        [{:fx/type     :scene.control/label
          :style-class ["label"]
          :text        (name (c/label cell))}])}]}))

(defn- add-connectors [component]
  (let [cells (set (fx/lookup component ".cell-box"))]
    (-> component
        .getChildren
        (.addAll
         (for [cell cells]
           (let [{:keys [min-x min-y width height]} (fx/bounds-in-parent cell)]
             (fx/make-tree
              {:fx/type :scene.shape/rectangle
               :x       min-x
               :y       min-y
               :width   width
               :height  height})))))
    component))

(defn- source-chain [start-cell]
  (let [cell-chain (cell-chain start-cell)]
    (-> {:fx/type :scene/group
         :children
         (for [[idx cell] (map-indexed vector cell-chain)]
           (cell-box cell [0 (* 65 idx)]))}
        fx/make-tree
        add-connectors
        )))

(defn- cells-graph-elements [cells]
  (let [;;sources (->> cells :meta (filter (fn [[id meta]] (get (:roles meta) :source))) (map first))
        sources (->> cells :cells keys (filter #(has-role? % :source)))]
    (def cc cells)
    (for [source sources] (source-chain source))))

(defn cells-graph
  [cells-atom]
  (let [watcher-name (gensym :graph-view)
        component
        (view/configure-view
         {:focused? true
          :component
          (with-status-line
            (fx/make-tree
             {:fx/type     :scene.control/scroll-pane
              :style-class ["scroll-pane" "main-component"]
              :content     {:fx/type     :scene/group
                            :style-class ["graph-group"]
                            :children    (cells-graph-elements @cells-atom)}})
            "cells!")})]
    (add-watch cells-atom watcher-name
               (fn [_ _ _ new]
                 (let [group (-> component (fx/lookup ".graph-group") first)
                       graph (cells-graph-elements new)]
                   (fx/run-later!
                    #(fx/set-field! group :children graph)))))

    (fx/set-field!
     component
     :fx/prop-listener [:visible (fn [source observable old visible] ;;this only works because datacore.ui.windows/replace! sets the visibility to false
                                   (prn '--watcher-removed watcher-name)
                                   (if-not visible
                                     (remove-watch cells-atom watcher-name)))])
    component))

;; interactive

(defin show-graph
  {:alias :cells/show-graph}
  []
  (let [component (cells-graph @#'c/global-cells)]
    (fx/run-later!
     #(windows/replace-focused! component))))
