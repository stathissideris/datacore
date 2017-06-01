(ns datacore.ui.util
  (:require [datacore.ui.java-fx :as fx]
            [datacore.cells :as c])
  (:import [javafx.util Callback]
           [javafx.stage StageStyle]
           [java.util Date]
           ;;[org.scenicview ScenicView]
           ))

(defn callback [fun]
  (reify Callback
    (call [this p]
      (fun p))))

(defn with-status-line [c label]
  (fx/make
   :scene.layout/border-pane
   {:center c
    :bottom (fx/make-tree
             {:fx/type :scene.control/label
              :text    label
              :style   "-fx-padding: 0.6em 0.6em 0.6em 0.6em;"})}))

(comment
  (defn scenic-view [node]
    (ScenicView/show node)))

(defn inspect [cell]
  (fx/run-later!
   (fn []
     (fx/make
      :stage/stage
      [[:title "datacore component inspector"]
       [:scene
        (fx/make
         :scene/scene
         {:fx/args [(fx/make
                     :scene.layout/border-pane
                     {:center cell
                      :bottom (c/formula (fn [_] (fx/label (Date.) {:style "-fx-padding: 0.5em;"})) cell)})]})]
       [:fx/setup fx/show!]]))))

(defn main-component [component]
  (some-> component
          (fx/find-by-style-class "main-component")
          (first)))

(defn focus-indicator-parent [component]
  (some->> component fx/parents (filter #(fx/has-style-class? % "focus-indicator")) first))

(defn focus-parent [component]
  (some->> component fx/parents (filter #(or (fx/has-style-class? % "focus-indicator")
                                             (fx/has-style-class? % "focus-parent"))) first))
