(ns datacore.error
  (:import [java.util Date]))

(def exceptions (atom []))
(defn exception [e]
  (swap! exceptions conj {:time (Date.)
                          :exception e})
  (throw e))

(defn clear! []
  (reset! exceptions []))
