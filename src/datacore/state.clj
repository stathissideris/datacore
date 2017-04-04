(ns datacore.state
  (:require [datacore.cells :as c :refer [cell defcell deformula]]
            [datacore.util :as util]
            [clojure.walk :as walk]))

(defcell view-to-component {})
(defcell layout-tree nil)

(defcell window->focused-component {})
(defcell focus nil)

(defn- assign-layout-ids [tree]
  (walk/postwalk
   (fn [x]
     (if (and (map? x) (:type x) (not (:id x)))
       (assoc x :id (str (java.util.UUID/randomUUID)))
       x))
   tree))

(defn swap-layout! [fun & args]
  (let [old-tree          (c/value layout-tree)
        new-tree          (-> (apply fun old-tree args)
                              (update :children vec))
        new-tree-with-ids (assign-layout-ids new-tree)]
    (c/reset! layout-tree new-tree-with-ids)))

;; {:type ::top-level
;;  :children
;;  [{:type ::view/window
;;     :title "datacore"
;;     :dimensions [100 800]
;;     :root
;;     {:type        ::view/split-pane
;;      :orientation :horizontal
;;      :children    [{:type        ::view/split-pane
;;                     :orientation :vertical
;;                     :children    [(CellId. 2)
;;                                   (CellId. 4)]}
;;                    {:type        ::view/split-pane
;;                     :orientation :vertical
;;                     :children    [(CellId. 3)
;;                                   ::view/nothing]}]}}]}

(comment
  (do
    (require '[datacore.source.csv :as csv])
    (require '[datacore.ui.java-fx :as fx])
    (def csv (csv/file {:filename "test-resources/watchlist.csv"}))
    (def csv-view (csv/default-view csv))
    )

  ;;show cell in window
  (swap-layout! assoc-in [:children 0 :root] {:type :datacore.ui.view/cell
                                              :cell csv-view})

  ;;open extra window
  (swap-layout! update :children conj
                {:type       :datacore.ui.view/window
                 :title      "datacore 200"
                 :dimensions [1000 800]
                 :root       {:type :datacore.ui.view/nothing}})

  ;;open prompt window
  (swap-layout! update :children conj
                {:type         :datacore.ui.view/window
                 :window-style :transparent
                 :root         {:type :datacore.ui.view/prompt}})

  ;;close last window
  (swap-layout! update :children (comp vec butlast))

  (defmacro simple-cell [name expr]
    `(c/deformula ~name
       (fn [data#]
         (update data# :data
                 (fn [~'data] ~expr)))
       ::c/unlinked))

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
