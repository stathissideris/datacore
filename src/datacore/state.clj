(ns datacore.state
  (:require [datacore.cells :as c :refer [cell defcell deformula]]
            [datacore.util :as util]
            [datacore.ui.keys.maps :as keymaps]))

(c/defcell focused-component nil)
(c/deformula effective-keymap
  (fn [keymaps focused]
    (prn 'keymap (some->> focused util/meta :datacore.ui.view/type))
    (keymaps/merge
     keymaps/root-keymap
     (some->> focused util/meta :datacore.ui.view/type (get (c/value keymaps/keymaps)))))
  keymaps/keymaps
  focused-component)

(require '[datacore.ui.view :as view])

(comment
  (do
  (require '[datacore.ui.java-fx :as fx])
  (require '[datacore.ui.windows :as windows])
  (require '[datacore.source.csv :as csv])
  (require '[datacore.ui.java-fx :as fx])
  (def csv (csv/file {:filename "test-resources/watchlist.csv"}))
  (def csv-view (csv/default-view csv))
  )

  (windows/new-window)

  ;;show cell in window
  (fx/run-later!
   #(windows/replace-focused!
     (fx/make-tree
      (view/build-view
       {::view/type ::view/cell
        :cell       csv-view
        :focused?   true}))))

  (do
    (require '[datacore.ui.view.web :as web])
    (c/defcell web-input {})
    (def web-view (web/view web-input)))
  (windows/replace-focused!
   (fx/make-tree
    (view/build-view
     {::view/type ::view/cell
      :cell       web-view
      :focused?   true})))
  (c/reset! web-input {:url "http://www.imdb.com/title/tt0088247/"})
  (c/reset! web-input {:url "http://www.imdb.com/title/tt0478126/"})
  (c/reset! web-input {:url "http://chart.apis.google.com/chart?cht=bvs&chs=500x250&chd=t:100,200,300,400,500,600,700&chds=0,700&chl=Savings||Checking||Money"})
  (c/reset! web-input {:url "https://www.google.com/maps/embed?pb=!1m14!1m12!1m3!1d19849.480026927366!2d-0.1058117!3d51.546506550000004!2m3!1f0!2f0!3f0!3m2!1i1024!2i768!4f13.1!5e0!3m2!1sen!2suk!4v1496623719065"})
  (c/reset! web-input {:content (slurp "/Users/sideris/devel/dali/examples/output/architecture.svg")})
  (c/reset! web-input {:content "<html><body><h1>Test</h1><p>This is a <i>test</i>.</p></body></html>"})
  (c/reset! web-input {:content "<iframe src=\"https://www.google.com/maps/embed?pb=!1m14!1m12!1m3!1d19849.480026927366!2d-0.1058117!3d51.546506550000004!2m3!1f0!2f0!3f0!3m2!1i1024!2i768!4f13.1!5e0!3m2!1sen!2suk!4v1496623719065\" width=\"600\" height=\"450\" frameborder=\"0\" style=\"border:0\" allowfullscreen></iframe>"})

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
