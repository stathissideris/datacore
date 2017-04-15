(ns datacore.ui.view.web
  (:require [datacore.ui.view :as view]
            [datacore.ui.java-fx :as fx]
            [datacore.cells :as c]
            [datacore.ui.util :refer [with-status-line]]))

(defn- load [engine {:keys [url content content-type] :as input}]
  (cond url
          (fx/run-later! #(.load engine url))
        (and content content-type)
          (fx/run-later! #(.loadContent engine content content-type))
        content
          (fx/run-later! #(.loadContent engine content))
        :else nil))

(defmethod view/build-view :datacore.ui.view/web
  [view-cell]
  (let [view   (fx/make :scene.web/web-view)
        engine (.getEngine view)]
    (load engine (c/value view-cell))
    (c/add-watch! view-cell :web-view (fn [_ _ new] (load engine new)))
    (with-status-line
      view (c/formula (fn [{:keys [url content content-type]}]
                        (str "Web View: "
                             (cond url url
                                   content (str "Local content" content-type)
                                   :else "Nothing loaded")))
                      view-cell
                      {:label :web-view-status-line}))))

(defn view [web-input]
  (c/formula
   (fn [input]
     (merge
      input
      {:type :datacore.ui.view/web}))
   web-input
   {:label :web-view}))
