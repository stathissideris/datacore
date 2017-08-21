(ns datacore.ui.view.edn
  (:require [datacore.cells :as c]
            [datacore.ui.view :as view]
            [clojure.pprint :refer [pprint]]
            [datacore.ui.util :refer [with-status-line]]
            [datacore.ui.java-fx :as fx]))

(defn view-cell [source-cell]
  (c/formula
   (fn [data] {::view/type ::view/edn
               :data       data})
   source-cell
   {:label :edn-view}))

(defmethod view/build-cell-view ::view/edn
  [view-cell]
  (view/configure-view
   {:cell      view-cell
    :component
    (with-status-line
      (fx/make-tree
       {:fx/type     :scene.control/text-area
        :style-class ["edn-view" "text-area" "main-component"]
        :text        (c/formula (fn [x] (with-out-str
                                          (pprint (:data x))))
                                view-cell
                                {:label :edn-data
                                 :meta  {:roles #{:system}}})})
      "EDN!")}))
