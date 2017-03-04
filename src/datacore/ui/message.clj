(ns datacore.ui.message
  (:require [datacore.cells :as c :refer [cell cell=]]))

(def ^:private current-message-input (cell "Welcome"))
(def current-message (cell= @current-message-input))

(defn message [x]
  (c/swap! current-message-input (fn [_] x)))

(defn error [x]
  (c/swap! current-message-input (fn [_] x)))
