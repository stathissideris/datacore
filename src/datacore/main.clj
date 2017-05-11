(ns datacore.main
  (:require [clojure.repl :as repl]
            [datacore.cells :as c]
            [datacore.state :as state]
            [datacore.ui.view :as view]
            [datacore.ui.java-fx :as fx]
            [datacore.ui.windows :as windows]))

(defmacro with-err-str
  "Evaluates exprs in a context in which *err* is bound to a fresh
  StringWriter.  Returns the string created by any nested printing
  calls."
  {:added "1.0"}
  [& body]
  `(let [s# (new java.io.StringWriter)]
     (binding [*err* s#]
       ~@body
       (str s#))))

(def uncaught-exception (atom nil))
(defn- global-exception-handler []
  (reify Thread$UncaughtExceptionHandler
    (uncaughtException [this thread throwable]
      (reset! uncaught-exception throwable)
      (let [trace (with-err-str (repl/pst throwable 150))]
        (println "UNCAUGHT EXCEPTION" trace)))))

(defn init []
  ;;(set! *warn-on-reflection* true)
  (fx/init)

  (Thread/setDefaultUncaughtExceptionHandler (global-exception-handler))

  (c/add-watch! state/layout-tree :layout-watch (fn [_ old new] (@#'view/update-layout! old new)))

  (windows/new))
