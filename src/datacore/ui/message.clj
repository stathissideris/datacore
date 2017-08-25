(ns datacore.ui.message
  (:require [datacore.cells :as c]
            [datacore.ui.timer :as timer]))

(def initial-state
  {:type :message
   :msg  "Ready"})

(def message-input
  (c/cell :message-input initial-state
          {:meta {:roles #{:system}}}))
(def current-message
  (c/formula identity message-input
             {:label :current-message
              :meta {:roles #{:system}}}))
(def timer (atom nil))

(defn message [x]
  (c/reset! message-input {:type :message
                           :msg  (str x)})
  (when-let [t @timer] (timer/cancel t))
  (reset! timer (timer/delayed 3000 #(c/reset! message-input initial-state))))

(defn error [x]
  (c/reset! message-input {:type :error
                           :msg  (str x)})
  (when-let [t @timer] (timer/cancel t))
  (reset! timer (timer/delayed 3000 #(c/reset! message-input initial-state))))
