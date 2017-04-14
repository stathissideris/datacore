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

(defin delete
  {:alias :windows/delete}
  []
  (println :delete))

(defin balance
  {:alias :windows/balance}
  []
  (println :balance))

;;split

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

;;focus

(defin focus-left
  {:alias :windows/focus-left}
  [])

(defin focus-right
  {:alias :windows/focus-right}
  [])

(defin focus-up
  {:alias :windows/focus-up}
  [])

(defin focus-down
  {:alias :windows/focus-down}
  [])

;;swap

(defin swap-left
  {:alias :windows/swap-left}
  [])

(defin swap-right
  {:alias :windows/swap-right}
  [])

(defin swap-up
  {:alias :windows/swap-up}
  [])

(defin swap-down
  {:alias :windows/swap-down}
  [])
