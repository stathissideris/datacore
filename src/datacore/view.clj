(ns datacore.view
  (:require [datacore.ui.java-fx :as fx]
            [datacore.cells :as c]))

(defmulti build-view (fn [x] (or (:type x)
                                 (when (c/cell-id? x) (:type (c/value x)))
                                 ::existing)))

(defmethod build-view ::nothing
  [_]
  (fx/make :scene.control/label {:text "Nothing to show"}))

(defmethod build-view ::split-pane
  [{:keys [orientation children]}]
  (fx/make :scene.control/split-pane
           {:items       (map build-view children)
            :orientation (if (= orientation :horizontal)
                           javafx.geometry.Orientation/HORIZONTAL
                           javafx.geometry.Orientation/VERTICAL)}))

(defmethod build-view ::existing
  [{:keys [component view]}]
  (if component
    component
    (fx/make :scene.control/label
             {:text (str "ERROR! component for view " view " does not exist")})))
