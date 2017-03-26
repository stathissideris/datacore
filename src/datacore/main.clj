(ns datacore.main
  (:require [clojure.repl :as repl]))

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
  (Thread/setDefaultUncaughtExceptionHandler (global-exception-handler)))

(init)
