(ns datacore.main
  (:require [clojure.repl :as repl]
            [datacore.cells :as c]
            [datacore.state :as state]
            [datacore.ui.view :as view]
            [datacore.ui.java-fx :as fx]))

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

(defn- global-exception-handler []
  (reify Thread$UncaughtExceptionHandler
    (uncaughtException [this thread throwable]
      (let [trace (with-err-str (repl/pst throwable 150))]
        (println "UNCAUGHT EXCEPTION" trace)))))

(defn init []
  ;;(set! *warn-on-reflection* true)
  (fx/init)

  (Thread/setDefaultUncaughtExceptionHandler (global-exception-handler))

  (c/add-watch! state/layout-tree :layout-watch (fn [_ old new] (@#'view/update-layout! old new)))

  (state/swap-layout!
   (constantly
    {:type     :datacore.ui.view/top-level
     :children []}))
  (state/swap-layout! update :children conj
                      {:type       :datacore.ui.view/window
                       :title      "datacore"
                       :dimensions [1000 800]
                       :root       {:type       :datacore.ui.view/nothing
                                    :focused?   true
                                    :focusable? true}}))
