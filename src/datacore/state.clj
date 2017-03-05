(ns datacore.state
  (:require [datacore.cells :as c :refer [cell defcell deformula]]))

(defcell state {})
(deformula sources :sources state)
(deformula views :views state)

(defn add-source [state source]
  (update state :sources conj source))

(defn add-view [state {:keys [source] :as view}]
  (when-not (seq (filter #(= % source) (:sources state)))
    (throw (ex-info "Cannot add view because requested data source does not exist"
                    {:state state
                     :view view
                     :source source})))
  (update state :views (fnil conj []) view))

(defn add-transformer [state view-label transformer]
  (update state :views
          (fn [views]
            (mapv (fn [{:keys [label] :as view}]
                    (if (= label view-label)
                      (update view :transformers c/swap! conj transformer)
                      view))
                  views))))

(defn clear-transformers [state view-label]
  (update state :views
          (fn [views]
            (mapv (fn [{:keys [label] :as view}]
                    (if (= label view-label)
                      (assoc view :transformers c/reset! [])
                      view))
                  views))))

(comment
  (do
    (require '[datacore.source.csv :as csv])
    (require '[datacore.ui.java-fx :as fx])
    (fx/run-later! datacore.ui/make-app)
    (def s (csv/make {:filename "/Users/sideris/devel/pixelated-noise/data/all-barclays.tsv"
                      :separator \tab}))
    (c/swap! state add-source s)
    (c/swap! state add-view (csv/default-view s))

    (-> @state :views first :label (c/reset! "this is a new label"))
    (c/swap! (-> @state :views first :transformers)
             conj
             {:label    "business only"
              :function (fn [data] (filter #(= (nth % 4) "business-main") data))})

    (c/swap! state add-transformer "all-barclays.tsv"
             {:label    "business only"
              :function (fn [data] (filter #(= (nth % 4) "business-main") data))})

    (c/swap! state clear-transformers "all-barclays.tsv")
    )
  )


(comment
  {:sources []
   :views   [{:source       source
              :label        ""
              :type         :table
              :transformers [{:label      ""
                              :expression ()
                              :function   ()
                              :active?    true}]}] ;;you can re-order them and turn them on/off
   })
