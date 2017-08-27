(ns datacore.source.edn
  (:require [clojure.edn :as edn]
            [me.raynes.fs :as fs]
            [datacore.cells :as c]
            [datacore.util :as util]
            [datacore.ui.view :as view]
            [datacore.ui.java-fx :as fx]
            [datacore.ui.windows :as windows]
            [datacore.ui.interactive :as in :refer [defin]]
            [hawk.core :as hawk]))

(defn file [{:keys [filename] :as options}]
  (when-not (fs/exists? filename)
    (throw (ex-info "File does not exist" {:filename filename})))
  (let [edn-cell (c/cell
                  :edn-file
                  {:data          (edn/read-string (slurp filename)) ;;TODO replace slurp
                   :label         (fs/base-name filename)
                   :filename      filename
                   :last-modified (util/time-in-millis)}
                  {:roles #{:source}})]
    (hawk/watch! [{:paths   [filename]
                   :handler
                   (fn [_ _]
                     (c/swap!
                      edn-cell
                      #(merge
                        %
                        {:data          (edn/read-string (slurp filename)) ;;TODO replace slurp
                         :last-modified (util/time-in-millis)})))}])
    edn-cell))

(defn default-view [edn-cell]
  (c/formula
   (fn [contents control]
     (merge
      contents
      `{::view/type ::view/edn}
      ;;TODO also uniquify label
      (assoc contents :control control)))
   edn-cell ::c/unlinked
   {:label :table-view}))

(defin load-edn
  {:alias  :edn/load-edn
   :help   [:span "Load an EDN file into an EDN view. The file is "
            [:i "watched"] " by default -- any changes to the file"
            " are reflected to the view automatically."]
   :params [[:filename {:type   ::in/file
                        :title  "Load EDN"
                        :prompt "Select EDN file to load"
                        :help   "The EDN file to load"}]]}
  [{:keys [filename]}]
  (let [edn-cell  (file {:filename filename})
        view-cell (default-view edn-cell)
        component (view/build-cell-view view-cell)]
    (fx/run-later! #(windows/replace-focused! component))))
