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
    (def csv (csv/cell {:filename "test-resources/watchlist.csv"}))
    (def csv-view (csv/default-view csv))
    (c/swap! state add-source csv)
    (c/swap! state add-view csv-view))

  (do
   (c/deformula filter-cell
     (fn [data]
       (update data :data
               (fn [rows] (filter #(= "Documentary" (nth % 6)) rows))))
     ::c/unlinked)

   (def _ (c/linear-insert! csv filter-cell csv-view))

   (def _
     (c/swap-function!
      filter-cell
      (fn [data]
        (update data :data
                (fn [rows] (filter #(= "Mini-Series" (nth % 6)) rows))))))

   (def _ (c/swap-function! filter-cell identity))

   ;; OR

   (def _ (c/mute! filter-cell))
   (def _ (c/unmute! filter-cell))


   (c/deformula sort-cell
     (fn [data]
       (update data :data (partial sort-by #(nth % 11))))
     ::c/unlinked)

   (def _ (c/linear-insert! csv sort-cell filter-cell))
   (def _ (c/mute! sort-cell))
   (def _ (c/unmute! sort-cell))
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
