(ns datacore.main
  (:require [datacore.cells :as c]
            [datacore.async :as async]
            [datacore.state :as state]
            [datacore.util :as util]
            [datacore.ui.view :as view]
            [datacore.ui.java-fx :as fx]
            [datacore.ui.windows :as windows]
            [datacore.ui.prompt]   ;;for the multimethods
            [datacore.ui.cells]    ;;for the interactive methods
            [datacore.source.csv]) ;;for the interactive methods
  (:gen-class :main true))

(defn- global-exception-handler []
  (reify Thread$UncaughtExceptionHandler
    (uncaughtException [_ _ throwable]
      (util/handle-uncaught throwable))))

(defn init []
  ;;(set! *warn-on-reflection* true)
  ;;(fx/set-app-icon "/images/rick.jpg") ;;TODO calling this seems to trigger this https://stackoverflow.com/questions/14977546/javafx-nullpointerexception-on-startup
  (async/start-sliding-future!)
  (fx/init)

  (Thread/setDefaultUncaughtExceptionHandler (global-exception-handler))

  (windows/new-window))

(defn -main
  [& args]
  (init)
  (Thread/sleep Long/MAX_VALUE))
