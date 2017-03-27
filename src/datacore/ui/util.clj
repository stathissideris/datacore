(ns datacore.ui.util
  (:require [datacore.ui.java-fx :as fx])
  (:import [javafx.util Callback]))

(defn callback [fun]
  (reify Callback
    (call [this p]
      (fun p))))

(defn with-status-line [c label]
  (fx/make
   :scene.layout/border-pane
   {:center c
    :bottom (fx/make :scene.control/label {:text label})}))
