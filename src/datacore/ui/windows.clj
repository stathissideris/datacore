(ns datacore.ui.windows
  (:require [clojure.walk :as walk]
            [clojure.zip :as zip]
            [datacore :refer [defin]]
            [datacore.util :as util]
            [datacore.ui.view :as view]
            [datacore.ui.timer :as timer]
            [datacore.ui.java-fx :as fx]
            [datacore.cells :as c]
            [datacore.state :as state]))

(defin maximize
  {:alias :windows/maximize}
  []
  (state/swap-layout!
   (fn [tree]
     (update
      tree :children
      (fn [ch]
        (map #(if-let [focused (view/find-focused %)]
                (assoc % :root focused)
                %) ch))))))

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

(defn- split-pane-index-of [item split-pane]
  (util/index-of item (fx/get-field split-pane :items)))

(defn- split [orientation]
  (let [focused    (view/focus-indicator-parent (fx/focus-owner))
        parent     (fx/parent focused)
        split-pane
        (fx/make-tree
         {:fx/type     :scene.control/split-pane
          :items       [focused (view/build-view {:type ::view/nothing})]
          :orientation (if (= orientation :horizontal)
                         javafx.geometry.Orientation/HORIZONTAL
                         javafx.geometry.Orientation/VERTICAL)})]
    (if (fx/has-style-class? parent "root")
      (fx/set-field! parent :center split-pane)
      (let [parent (fx/parent parent)
            idx    (split-pane-index-of focused parent)]
        (.set (.getItems parent) idx split-pane)))
    (timer/delayed 20 #(view/focus! focused))))

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

;; multiple windows

(defin new-window
  {:alias :windows/new}
  []
  (fx/run-later!
   #(fx/show!
     (fx/make-tree
      (view/build-view
       {:type       :datacore.ui.view/window
        :title      "datacore"
        :dimensions [1000 800]
        :root       (view/build-view
                     {:type       :datacore.ui.view/nothing
                      :focused?   true
                      :focusable? true})})))))
