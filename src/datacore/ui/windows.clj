(ns datacore.ui.windows
  (:require [clojure.walk :as walk]
            [datacore :refer [defin]]
            [datacore.ui.view :as view]
            [datacore.cells :as c]
            [datacore.state :as state]))

(defin maximize
  {:alias :windows/maximize}
  []
  (println :maximize))

(defn- split [orientation]
  (state/swap-layout!
   (fn [tree]
     (walk/postwalk
      (fn [{:keys [focused?] :as item}]
        (if focused?
          {:type        ::view/split-pane
           :orientation orientation
           :children [item
                      {:type ::view/nothing}]}
          item))
      tree))))

(defin split-below
  {:alias :windows/split-below}
  []
  (split :vertical))

(defin split-right
  {:alias :windows/split-right}
  []
  (split :horizontal))

(defin delete
  {:alias :windows/delete}
  []
  (println :delete))
