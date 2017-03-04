(ns datacore.source.csv
  (:require [clojure.data.csv :as csv]
            [clojure.java.io :as io]
            [me.raynes.fs :as fs]))

(defn load-csv [{:keys [filename separator quote] :as options}]
  (with-open [in-file (io/reader filename)]
    (doall
     (apply csv/read-csv in-file (mapcat identity options)))))

(defn make [{:keys [filename] :as options}]
  (when-not (fs/exists? filename)
    (throw (ex-info "File does not exist" {:filename filename})))
  {:label    (fs/base-name filename)
   :filename filename
   :data-fn  #(load-csv options)})

(defn default-view [{:keys [label] :as source}]
  {:source source
   :label  label
   :type   :datacore.view/table})
