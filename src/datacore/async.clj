(ns datacore.async
  (:require [clojure.core.async :as async]
            [datacore.ui.timer :as timer]))

;;;;;;;; sliding future ;;;;;;;;

(defonce sliding-future-chan nil)

(defn stop-sliding-future! []
  (when sliding-future-chan
    (async/put! sliding-future-chan ::close)))

(defn start-sliding-future! []
  (stop-sliding-future!)
  (alter-var-root #'sliding-future-chan (constantly (async/chan (async/sliding-buffer 8))))
  (async/thread
    (loop []
      (let [fun (async/<!! sliding-future-chan)]
        (if (= ::close fun)
          (async/close! sliding-future-chan)
          (do
            (fun)
            (recur)))))))

(defn sliding-future [fun]
  (async/offer! sliding-future-chan fun))

;;;;;;;; throttled ;;;;;;;;

(defn throttled
  "Ignores too frequent calls (defined by millis), but if you stop
  calling it, it waits for millis and then calls the function. One-off
  infrequent calls happen without delay.

  This is useful to suppressing state updates until the UI stops
  getting hammered by frequent user input (for example, pressing and
  holding up/down arrows in a table to move between rows)."
  [millis fun]
  (let [timer (atom nil)]
    (fn [& args]
      (let [t @timer]
        (timer/cancel t)
        (reset! timer (timer/delayed millis #(apply fun args)))
        (when-not t (apply fun args))))))

;;(def foo (throttled 2000 (fn [] (prn 'foo!))))
