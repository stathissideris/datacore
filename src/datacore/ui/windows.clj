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
           :children    [item
                         {:type       ::view/nothing
                          :focusable? true}]}
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

(defn- focus-to-direction [direction]
  (let [tree     (c/value state/layout-tree)
        focused  (view/find-focused tree)
        focus-id (view/component-in-direction (:id focused) direction tree)]
    (prn 'will-focus focus-id)
    (when focus-id
      (state/swap-layout!
       (fn [tree]
         (walk/postwalk
          (fn [{:keys [id] :as x}]
            (if (map? x)
              (if (= id focus-id)
                (assoc x :focused? true)
                (dissoc x :focused?))
              x))
          tree))))))

(defin focus-left
  {:alias :windows/focus-left}
  []
  (focus-to-direction :left))

(defin focus-right
  {:alias :windows/focus-right}
  []
  (focus-to-direction :right))

(defin focus-up
  {:alias :windows/focus-up}
  []
  (focus-to-direction :up))

(defin focus-down
  {:alias :windows/focus-down}
  []
  (focus-to-direction :down))

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
