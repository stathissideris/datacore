(ns datacore.state
  (:require [datacore.cells :as c :refer [cell cell=]]))

(def state (cell {}))
(def sources (cell= (:sources state)))
(def views (cell= (:views state)))

(defn add-source [state source]
  (update state :sources conj source))

(defn add-view [state {:keys [source] :as view}]
  (when-not (seq (filter #(= % source) (:sources state)))
    (throw (ex-info "Cannot add view because requested data source does not exist"
                    {:state state
                     :view view
                     :source source})))
  (update state :views conj view))

(comment
  (do
    (require '[datacore.source.csv :as csv])
    (datacore.ui/run-later! datacore.ui/make-app)
    (def s (csv/make {:filename "/Users/sideris/devel/pixelated-noise/data/all-barclays.tsv"
                      :separator \tab}))
    (c/swap! state add-source s)
    (c/swap! state add-view (csv/default-view s))
    ))


(comment
  {:sources []
   :views   [{:source       source
              :name         ""
              :type         :table
              :transformers [{:expression ()
                              :function   ()
                              :active     true}]}] ;;you can re-order them and turn them on/off
   })
