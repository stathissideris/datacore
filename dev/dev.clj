(ns dev
  (:require [clojure.tools.namespace.repl :as repl :refer (clear refresh-all)]))

(defn refresh []
  (repl/refresh))

#_(defn run-all-my-tests []
    (reset)
    (test/run-all-tests #"osio.website-publisher.*test$"))

#_(defn run-selected-tests [& namespaces]
    (reset)
    (apply test/run-tests namespaces))
