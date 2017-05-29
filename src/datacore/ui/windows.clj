(ns datacore.ui.windows
  (:require [clojure.walk :as walk]
            [clojure.zip :as zip]
            [datacore.ui.interactive :refer [defin]]
            [datacore.util :as util]
            [datacore.ui.util :as ui-util]
            [datacore.ui.view :as view]
            [datacore.ui.timer :as timer]
            [datacore.ui.java-fx :as fx]
            [datacore.cells :as c]))

(defn- focus-owner []
  (ui-util/focus-indicator-parent
   (fx/focus-owner @view/focused-stage)))

(defn- split-pane-index-of [item split-pane]
  (when item
    (util/index-of item (fx/get-field split-pane :items))))

(defn- replace-in-split-pane! [split-pane old new]
  (when (and split-pane old new)
    (let [idx (split-pane-index-of old split-pane)]
      (when-not idx
        (throw (ex-info "Cannot find component in split-pane"
                        {:old-component (fx/tree old)
                         :new-component (fx/tree new)
                         :split-pane    (fx/tree split-pane)})))
      (.set (.getItems split-pane) (int idx) new))))

(defn- replace! [reference-component new-component]
  (let [parent (fx/parent reference-component)]
    (when parent
      (fx/run-later!
       #(if (fx/has-style-class? parent "root")
          (fx/set-field! parent :center new-component)
          (replace-in-split-pane! (fx/parent parent) reference-component new-component))))))

(defn replace-focused! [component]
  (replace! (focus-owner) component))

(defn- get-root [component]
  (some->> (fx/parents component) (filter #(fx/has-style-class? % "root")) first))

(defin maximize
  {:alias :windows/maximize}
  []
  (let [focused (focus-owner)
        parent  (fx/parent focused)]
    (when-not (fx/has-style-class? parent "root")
      (let [root (get-root focused)]
        (fx/run-later! #(fx/set-field! root :center focused))))))

(defin delete
  {:alias :windows/delete}
  []
  (let [focused (focus-owner)
        parent  (fx/parent focused)]
    (when-not (fx/has-style-class? parent "root")
      (let [split-pane (fx/parent parent)
            other      (some->> (.getItems split-pane) (remove #(= % focused)) first)]
        (replace! split-pane other)))))

(defin balance
  {:alias :windows/balance}
  []
  (let [root (get-root (focus-owner))]
    (doseq [sp (filter (partial instance? javafx.scene.control.SplitPane) (fx/tree-seq root))]
      (some-> sp .getDividers seq first (.setPosition 0.5)))))

;;split

(defn- split [orientation]
  (let [focused    (focus-owner)
        split-pane
        (fx/make-tree
         {:fx/type     :scene.control/split-pane
          :items       [focused (view/build-view {:type ::view/nothing})]
          :orientation (if (= orientation :horizontal)
                         javafx.geometry.Orientation/HORIZONTAL
                         javafx.geometry.Orientation/VERTICAL)})]
    (replace! focused split-pane)
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
  (view/focus! (view/focusable-in-direction (focus-owner) direction)))

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
  (let [focused (focus-owner)
        other   (view/focusable-in-direction focused direction)]
    (fx/run-later!
     #(when (and focused other)
        (let [this-sp   (-> focused fx/parent fx/parent)
              other-sp  (-> other fx/parent fx/parent)
              this-idx  (split-pane-index-of focused this-sp)
              other-idx (split-pane-index-of other other-sp)]
          (if (#{:right :down} direction)
            (do
              (.set (.getItems other-sp) other-idx focused)
              (.set (.getItems this-sp) this-idx other))
            (do
              (.set (.getItems this-sp) this-idx other)
              (.set (.getItems other-sp) other-idx focused)))
          (view/focus! focused))))))

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
