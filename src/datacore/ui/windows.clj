(ns datacore.ui.windows
  (:require [datacore :refer [defin]]
            [datacore.view :as view]
            [datacore.cells :as c]
            [datacore.state :as state]))

(defin maximize
  {:alias :windows/maximize}
  []
  (println :maximize))

(defin split-below
  {:alias :windows/split-right}
  []
  (c/swap!
   state/layout-tree
   (fn [tree]
     {:type        ::view/split-pane
      :orientation :vertical
      :children [tree
                 {:type ::view/nothing}]})))

(defin split-right
  {:alias :windows/split-right}
  []
  (c/swap!
   state/layout-tree
   (fn [tree]
     {:type        ::view/split-pane
      :orientation :horizontal
      :children [tree
                 {:type ::view/nothing}]})))

(defin delete
  {:alias :windows/delete}
  []
  (println :delete))
