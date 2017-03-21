(ns datacore.state
  (:require [datacore.cells :as c :refer [cell defcell deformula]]))

(defcell state {})
(deformula views :views state)

(defn add-source [state source]
  (update state :sources (fnil conj []) source))

(defn add-view [state {:keys [source] :as view}]
  (update state :views (fnil conj []) view))

(comment
  (do
    (require '[datacore.source.csv :as csv])
    (require '[datacore.ui.java-fx :as fx])
    (fx/run-later! datacore.ui/make-app)
    (def csv (csv/cell {:filename "/Users/sideris/devel/pixelated-noise/data/all-barclays.tsv"
                        :separator \tab}))
    (def csv-view (csv/default-view csv))
    (c/swap! state add-source csv)
    (c/swap! state add-view csv-view)
    (def _ (doall (c/value csv-view)))

    (c/deformula filter-cell
      (fn [data]
        (update data :data
                (fn [rows] (filter #(= "business-main" (nth % 4)) rows))))
      ::c/unlinked)

    (c/linear-insert! csv filter-cell csv-view)

    (def _
      (c/swap-function!
       filter-cell
       (fn [data]
         (update data :data
                 (fn [rows] (filter #(= "business-saver" (nth % 4)) rows))))))

    (def _
      (c/swap-function!
       filter-cell
       (fn [data]
         (update data :data
                 (fn [rows] (filter #(= "CASH" (nth % 2)) rows))))))

    (def _
      (c/swap-function! filter-cell identity))

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
