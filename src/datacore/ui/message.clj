(ns datacore.ui.message
  (:require [datacore.cells :as c]
            [datacore.ui.timer :as timer]))

(def ^:private message-input (c/cell :message-input "Ready"))
(c/deformula current-message str message-input)
(def timer (atom nil))

(defn message [x]
  (c/swap! message-input (fn [_] x))
  (when-let [t @timer] (timer/cancel t))
  (reset! timer (timer/delayed 3000 #(c/reset! message-input nil))))

(defn error [x]
  (c/swap! message-input (fn [_] x))
  (when-let [t @timer] (timer/cancel t))
  (reset! timer (timer/delayed 3000 #(c/reset! message-input nil))))
