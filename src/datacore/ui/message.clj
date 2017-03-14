(ns datacore.ui.message
  (:require [datacore.cells :as c :refer [cell formula]]))

;; (def ^:private current-message-input (cell "Welcome"))
;; (def current-message (formula identity current-message-input))

(def ^:private current-message-input nil)
(def current-message nil)

(defn message [x]
  (c/swap! current-message-input (fn [_] x)))

(defn error [x]
  (c/swap! current-message-input (fn [_] x)))
