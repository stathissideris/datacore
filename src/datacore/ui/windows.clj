(ns datacore.ui.windows
  (:require [clojure.walk :as walk]
            [clojure.zip :as zip]
            [datacore :refer [defin]]
            [datacore.ui.view :as view]
            [datacore.cells :as c]
            [datacore.state :as state]))

(defin maximize
  {:alias :windows/maximize}
  []
  (println :maximize))

(defn- focus-first-focusable [tree]
  (loop [z (view/layout-zipper tree)]
    (let [node (zip/node z)]
      (cond (zip/end? z)
            (zip/root z)
            (:focusable? node)
            (zip/root (zip/replace z (assoc node :focused? true)))
            :else
            (recur (zip/next z))))))

(defin delete
  {:alias :windows/delete}
  []
  (let [tree    (c/value state/layout-tree)
        focused (view/find-focused tree)]
    (state/swap-layout!
     (fn [tree]
       (walk/postwalk
        (fn [item]
          (if (map? item)
            (if-let [to-delete (some->> item :children (filter #(= (:id focused) (:id %))) first)]
              (let [other (some->> item :children (remove #(= (:id focused) (:id %))) first)]
                (focus-first-focusable other))
              item)
            item))
        tree)))))

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
        focused  (view/find-focused tree)]
    (view/focus! (view/node-in-direction (:id focused) direction tree))))

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

(defn- swap-to-direction [direction]
  (let [tree    (c/value state/layout-tree)
        focused (view/find-focused tree)
        swap-id (when focused (view/node-in-direction (:id focused) direction tree))
        swapped (when swap-id (view/find-by-id tree swap-id))]
    (when (and focused swap-id)
      (state/swap-layout!
       (fn [tree]
         (walk/postwalk
          (fn [{:keys [id] :as x}]
            (if (map? x)
              (cond (= id swap-id) focused
                    (= id (:id focused)) swapped
                    :else x)
              x))
          tree))))))

(defin swap-left
  {:alias :windows/swap-left}
  []
  (swap-to-direction :left))

(defin swap-right
  {:alias :windows/swap-right}
  []
  (swap-to-direction :right))

(defin swap-up
  {:alias :windows/swap-up}
  []
  (swap-to-direction :up))

(defin swap-down
  {:alias :windows/swap-down}
  []
  (swap-to-direction :down))
