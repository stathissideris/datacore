(ns user
  (:require [clojure.tools.namespace.repl :as ctn]))

(defn dev []
  (ctn/refresh)
  (in-ns 'dev))
