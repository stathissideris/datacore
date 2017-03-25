(ns datacore.source.csv
  (:require [clojure.data.csv :as csv]
            [clojure.java.io :as io]
            [me.raynes.fs :as fs]
            [datacore.util :as util]
            [datacore.cells :as c]
            [hawk.core :as hawk]))

(defn load-csv [{:keys [filename separator quote] :as options}]
  (with-open [in-file (io/reader filename)]
    (doall
     (apply csv/read-csv in-file (mapcat identity options)))))

(defn cell [{:keys [filename] :as options}]
  (when-not (fs/exists? filename)
    (throw (ex-info "File does not exist" {:filename filename})))
  (let [csv-cell (c/cell
                  :csv-file
                  {:label        (fs/base-name filename)
                   :filename     filename
                   :last-modifed (util/time-in-millis)
                   :data         (load-csv options)})]
    (hawk/watch! [{:paths   [filename]
                   :handler
                   (fn [_ _]
                     (c/swap!
                      csv-cell
                      #(-> %
                           (assoc :data (load-csv options))
                           (assoc :last-modified (util/time-in-millis)))))}])
    csv-cell))

(defn default-view [csv-cell]
  (c/formula
   (fn [{:keys [data label]}]
     {:data  data
      :label label
      :type  :datacore.view/table})
   csv-cell
   {:label :table-view}))
