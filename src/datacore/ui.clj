(ns datacore.ui
  (:require [datacore :refer [defin]]
            [datacore.ui.keys :as keys]
            [datacore.ui.keys.defaults :as default-keys]
            [datacore.ui.style :as style]
            [datacore.ui.message :as message]
            [datacore.view :as view]
            [datacore.ui.java-fx :as fx]
            [datacore.cells :as c]
            [datacore.state :as state])
  (:import [javafx.stage Stage]
           [javafx.scene.input KeyEvent]
           [javafx.event EventHandler Event]
           [javafx.scene.paint Color]))

(def scene nil)

(defn- message-line [message]
  (fx/make :scene.control/label
           {:text      (c/formula :msg message)
            :text-fill (c/formula (comp {:message Color/WHITE
                                         :error   (Color/web "0xF57000")}
                                        :type) message)}))

(defn build-layout [tree message]
  (fx/make
   :scene.layout/border-pane
   {:center (if-not tree
              (view/build-view ::view/nothing)
              (view/build-view tree))
    :bottom (message-line message)}))

(defn make-app []
  (let [the-scene   (fx/make :scene/scene
                             {:fx/args  [(build-layout ::view/nothing message/current-message) 800 800]
                              :fx/setup #(style/add-stylesheet % "css/default.css")})
        key-handler (keys/key-handler default-keys/root-keymap)]
    (alter-var-root #'scene (constantly the-scene))
    (c/add-watch!
     state/layout
     :layout-watch
     (fn [_ _ tree]
       (fx/run-later!
        #(.setRoot the-scene (build-layout tree message/current-message)))))
    (fx/make
     :stage/stage
     [[:scene    the-scene]
      [:title    "foobar"]
      [:fx/setup #(doto %
                    (.addEventFilter
                     KeyEvent/ANY
                     (reify EventHandler
                       (^void handle [this ^Event event]
                        (key-handler event))))
                    (.show))]])))

(defin execute-command
  {:alias :execute-command
   :params [{:type :text}]}
  [function-name]
  )

;;to see live CSS updates:

(comment
  (add-stylesheet @scene "file:///tmp/default.css"))
