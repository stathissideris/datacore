(ns dev
  (:require [clojure.tools.namespace.repl :as ns-tools]
            [mount.core :as mount]))

(defn refresh []
  (ns-tools/set-refresh-dirs "src/clj")
  (ns-tools/refresh))

(defn refresh-all []
  (ns-tools/set-refresh-dirs "src/clj")
  (ns-tools/refresh))

(defn start []
  (mount/start))
