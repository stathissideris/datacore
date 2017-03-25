(ns datacore.state
  (:require [datacore.cells :as c :refer [cell defcell deformula]]
            [datacore.util :as util]))

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
    (def csv (csv/file {:filename "test-resources/watchlist.csv"}))
    (def csv-view (csv/default-view csv))
    (c/swap! state add-source csv)
    (c/swap! state add-view csv-view)

    (defmacro simple-cell [name expr]
      `(c/deformula ~name
         (fn [data#]
           (update data# :data
                   (fn [~'data] ~expr)))
         ::c/unlinked)))

  (do
   (c/deformula filter-cell
     (fn [data]
       (update data :data
               (fn [rows] (filter #(= "Documentary" (:title-type %)) rows))))
     ::c/unlinked)
   ;;OR
   (simple-cell filter-cell (filter #(= "Documentary" (:title-type %)) data))

   (def _ (c/linear-insert! csv filter-cell csv-view))

   (def _
     (c/swap-function!
      filter-cell
      (fn [data]
        (update data :data
                (fn [rows] (filter #(= "Mini-Series" (:title-type %)) rows))))))

   (def _ (c/swap-function! filter-cell identity))

   ;; OR

   (def _ (c/mute! filter-cell))
   (def _ (c/unmute! filter-cell))


   (c/deformula sort-cell
     (fn [data]
       (update data :data (partial sort-by :year)))
     ::c/unlinked)
   ;;OR
   (simple-cell sort-cell (sort-by :year data))

   (def _ (c/linear-insert! csv sort-cell csv-view))
   (def _ (c/mute! sort-cell))
   (def _ (c/unmute! sort-cell))

   (c/deformula column-selector
     (fn [data]
       (let [columns [:title :year :title-type :directors :imdb-rating :genres]]
        (-> data
            (assoc :columns columns)
            (assoc :column-labels (zipmap columns ["Title" "Year" "Type" "Directors" "IMDB Rating" "Genres"])))))
     ::c/unlinked)

   (def _ (c/linear-insert! filter-cell column-selector csv-view))
   (def _ (c/linear-insert! csv column-selector csv-view))

   (c/deformula new-column
     (fn [data]
       (-> data
           (update :columns conj :fake-data)
           (update :data #(map (fn [row] (assoc row :fake-data (rand-int 10000))) %))))
     ::c/unlinked)

   (def _ (c/linear-insert! csv new-column csv-view))

   ))


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
