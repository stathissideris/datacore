(ns datacore.view
  (:require [datacore.ui.message :as message]))

(defmulti build-view :type)

(defn transform [data transformers]
  (try
   (if (empty? transformers)
     data
     (reduce (fn [data {:keys [function active?]}]
               (if active?
                 (function data)
                 data))
             data transformers))
   (catch Exception e
     (message/error "Error while applying transformers")
     (throw e))))
