(ns datacore.async
  (:require [clojure.core.async :as async]))

(defonce sliding-future-chan nil)

(defn close-sliding-future! []
  (when sliding-future-chan
    (async/put! sliding-future-chan ::close)))

(defn start-sliding-future! []
  (close-sliding-future!)
  (alter-var-root #'sliding-future-chan (constantly (async/chan (async/sliding-buffer 16))))
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
