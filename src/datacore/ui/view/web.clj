(ns datacore.ui.view.web
  (:refer-clojure :exclude [load])
  (:require [datacore.ui.view :as view]
            [datacore.ui.java-fx :as fx]
            [datacore.cells :as c]
            [datacore.ui.util :refer [with-status-line]]
            [datacore.ui.interactive :as in :refer [defin]])
  (:import [javafx.scene.input KeyEvent MouseEvent]))

(defn- load [engine {:keys [url content content-type] :as input}]
  (cond url
          (fx/run-later! #(.load engine url))
        (and content content-type)
          (fx/run-later! #(.loadContent engine content content-type))
        content
          (fx/run-later! #(.loadContent engine content))
        :else nil))

(defmethod view/build-cell-view :datacore.ui.view/web
  [view-cell]
  @(fx/run-later!
    #(let [view      (fx/make-tree
                      {:fx/type  :scene.layout/stack-pane
                       :style (str "-fx-border-width: 0 0 1 0;"
                                   "-fx-border-color: #c8c8c8;")
                       :children
                       [{:fx/type          :scene.web/web-view
                         :style-class      ["web-view" "main-component"]
                         :fx/stylesheet    "/css/default.css"
                         :fx/event-filter  [MouseEvent/MOUSE_CLICKED (fn [e] (view/focus! (.getTarget e)))]
                         :fx/link-listener (-> view-cell c/value :link-listener)}]})
           engine    (fx/get-field-in view [:children 0 :engine])
           _         (load engine (c/value view-cell))
           _         (c/add-watch! view-cell :web-view (fn [_ _ new] (load engine new)))
           component (-> (with-status-line
                           view (c/formula (fn [{:keys [url content title]}]
                                             (or title
                                                 (str "Web view: "
                                                      (cond url url
                                                            content "Local content"
                                                            :else "Nothing to show"))))
                                           view-cell
                                           {:label :web-view-status-line}))
                         (fx/set-field! :style-class ["focus-indicator"]))]
       (c/alter-meta! view-cell assoc :roles #{:view})
       (c/alter-meta! view-cell assoc :component component)
       component)))

(defn view [web-input]
  (c/formula
   (fn [input] (merge input {::view/type ::view/web}))
   web-input
   {:label :web-view}))

(defin scroll-up
  {:alias :web/scroll-up
   :params [[:component ::in/main-component]]}
  [{:keys [component]}]
  (fx/run-later!
   #(-> component .getEngine (.executeScript "window.scrollBy(0, -50);"))))

(defin scroll-down
  {:alias :web/scroll-down
   :params [[:component ::in/main-component]]}
  [{:keys [component]}]
  (fx/run-later!
   #(-> component .getEngine (.executeScript "window.scrollBy(0, 50);"))))

(defin scroll-to-top
  {:alias :web/scroll-to-top
   :params [[:component ::in/main-component]]}
  [{:keys [component]}]
  (fx/run-later!
   #(-> component .getEngine (.executeScript "window.scrollTo(0, 0);"))))

(defin scroll-to-bottom
  {:alias :web/scroll-to-bottom
   :params [[:component ::in/main-component]]}
  [{:keys [component]}]
  (fx/run-later!
   #(-> component .getEngine (.executeScript "window.scrollTo(0, document.body.scrollHeight);"))))
