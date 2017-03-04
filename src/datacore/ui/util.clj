(ns datacore.ui.util
  (:require [datacore.ui.java-fx :as fx]))

(defn status-line []
  (fx/make :scene.control/label {:text "status"}))

(defn with-status-line [c]
  (fx/make
   :scene.layout/border-pane
   {:center c
    :bottom (status-line)}))
