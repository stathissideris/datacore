(ns datacore.ui
  (:require [datacore.ui.keys :as keys]
            [datacore.ui.keys.defaults :as default-keys]
            [datacore.ui.style :as style]
            [datacore.ui.message :as message]
            [datacore.view :as view]
            [datacore.view.table]
            [datacore.ui.java-fx :as fx]
            [datacore.ui.util :as ui.util]
            [datacore.cells :as c]
            [datacore.state :as state])
  (:import [javafx.embed.swing JFXPanel]
           [javafx.stage Stage]
           [javafx.application Platform]
           [javafx.scene.input KeyEvent]
           [javafx.event EventHandler Event]))

;;(set! *warn-on-reflection* true)
(JFXPanel.)
(Platform/setImplicitExit false)

(defn build-layout [tree message]
  (fx/make
   :scene.layout/border-pane
   {:center (if-not tree
              (view/build-view ::view/nothing)
              (view/build-view tree))
    :bottom (fx/make :scene.control/label {:text message/current-message})}))

(def scene (atom nil))
(defn make-app []
  (let [the-scene   (fx/make :scene/scene
                             {:fx/args  [(build-layout ::view/nothing message/current-message) 800 800]
                              :fx/setup #(style/add-stylesheet % "css/default.css")})
        key-handler (keys/key-handler default-keys/root-keymap)]
    (c/add-watch!
     state/layout
     :layout-watch
     (fn [_ _ tree]
       (fx/run-later!
        #(.setRoot the-scene (build-layout tree message/current-message)))))
    (reset! scene the-scene)
    (comment
      (fx/make
       :stage/stage
       {:scene    the-scene
        :title    "foobar"
        :fx/setup #(.addEventFilter
                    % KeyEvent/ANY
                    (reify EventHandler
                      (^void handle [this ^Event event]
                       (key-handler event))))}))
    (doto (Stage.)
      (.setScene the-scene)
      (.setTitle "foobar")
      (.addEventFilter
       KeyEvent/ANY
       (reify EventHandler
         (^void handle [this ^Event event]
          (key-handler event))))
      (.show))))

;;to see live CSS updates:

(comment
  (add-stylesheet @scene "file:///tmp/default.css"))
