(ns datacore.ui.view.web
  (:refer-clojure :exclude [load])
  (:require [datacore.ui.view :as view]
            [datacore.ui.java-fx :as fx]
            [datacore.cells :as c]
            [datacore.ui.util :refer [with-status-line]])
  (:import [javafx.scene.input KeyEvent MouseEvent]))

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
  @(fx/run-later!
    #(let [view   (fx/make-tree
                   {:fx/type         :scene.web/web-view
                    :style-class     ["web-view" "main-component"]
                    :fx/event-filter [MouseEvent/MOUSE_CLICKED (fn [e] (view/focus! (.getTarget e)))]})
           engine (.getEngine view)]
       (load engine (c/value view-cell))
       (c/add-watch! view-cell :web-view (fn [_ _ new] (load engine new)))
       (-> (with-status-line
             view (c/formula (fn [{:keys [url content title]}]
                               (or title
                                   (str "Web view: "
                                        (cond url url
                                              content "Local content"
                                              :else "Nothing to show"))))
                             view-cell
                             {:label :web-view-status-line}))
           (fx/set-field! :style-class ["focus-indicator"])))))

(defn view [web-input]
  (c/formula
   (fn [input] (merge input {::view/type ::view/web}))
   web-input
   {:label :web-view}))
