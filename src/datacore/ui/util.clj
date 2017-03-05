(ns datacore.ui.util
  (:require [datacore.ui.java-fx :as fx]))

(defn with-status-line [c label]
  (fx/make
   :scene.layout/border-pane
   {:center c
    :bottom (fx/make :scene.control/label {:text label})}))
