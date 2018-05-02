(ns user
  (:require [clojure.tools.namespace.repl :as ns-tools]
            [expound.alpha :as expound]
            [clojure.spec.alpha :as s]))

(set! *warn-on-reflection* true)
(ns-tools/disable-unload!)

(defn load-dev []
  (alter-var-root #'s/*explain-out* (constantly expound/printer))
  (set! s/*explain-out* expound/printer)
  (require 'dev)
  (in-ns 'dev))

(defn dev []
  (ns-tools/refresh-all :after 'user/load-dev))
