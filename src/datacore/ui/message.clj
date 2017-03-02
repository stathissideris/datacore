(ns datacore.ui.message
  (:require [datacore.cells :as c :refer [cell]]))

(def current-message (cell "Welcome"))

(defn message [x]
  (c/swap! current-message (fn [_] x)))

(defn error [x]
  (c/swap! current-message (fn [_] x)))
