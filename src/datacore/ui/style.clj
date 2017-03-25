(ns datacore.ui.style
  (:require [hawk.core :as hawk])
  (:import [java.net URI]))

(def stylesheets (atom []))

(defn- clear-stylesheets [component]
  (-> component .getStylesheets .clear)
  (doseq [watcher (map :file-watcher @stylesheets)]
    (when watcher (hawk/stop! watcher)))
  (reset! stylesheets [])
  component)

(defn reload-stylesheets [component]
  (-> component .getStylesheets .clear)
  (-> component .getStylesheets (.addAll (map :path @stylesheets)))
  component)

(defn add-stylesheet [component path]
  (-> component .getStylesheets (.add path))
  (let [uri (URI. path)]
    (swap! stylesheets conj {:path path
                             :file-watcher
                             (when (.isAbsolute uri)
                               (hawk/watch! [{:paths   [(.getPath uri)]
                                              :handler (fn [_ _] (reload-stylesheets component))}]))}))
  component)
