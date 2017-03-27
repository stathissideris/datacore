(ns datacore.ui
  (:require [datacore :refer [defin]]
            [datacore.ui.keys :as keys]
            [datacore.ui.keys.defaults :as default-keys]
            [datacore.ui.style :as style]
            [datacore.ui.message :as message]
            [datacore.view :as view]
            [datacore.ui.java-fx :as fx]
            [datacore.cells :as c]
            [datacore.state :as state]
            [datacore.ui.observable :refer [observable-list]])
  (:import [javafx.stage Stage StageStyle]
           [javafx.scene.input KeyEvent]
           [javafx.event EventHandler Event]
           [javafx.scene.paint Color]
           [javafx.geometry Insets]
           [javafx.scene.layout Border CornerRadii BackgroundFill Background]
           [javafx.scene.effect DropShadow]
           [org.scenicview ScenicView]))

(def scene nil)

(defn- message-line [message]
  (fx/make :scene.control/label
           {:text      (c/formula :msg message)
            :text-fill (c/formula (comp {:message Color/BLACK
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
                              ;;:fx/setup #(style/add-stylesheet % "css/default.css")
                              })
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

(def popup nil)
(defn make-popup []
  ;;.centerOnScreen
  ;;.setOpacity
  (let [drop-shadow (fx/make :scene.effect/drop-shadow
                             {:radius 5.0
                              :offset-y 3.0})
        back        (Background.
                     (into-array
                      BackgroundFill
                      [(BackgroundFill.
                        (Color/color 0 0 0 0)
                        CornerRadii/EMPTY
                        Insets/EMPTY)]))
        s           (fx/make
                     :stage/stage
                     [;;[:fx/args [StageStyle/TRANSPARENT]]
                      [:title "foobar"]
                      [:always-on-top true]
                      [:scene
                       (fx/make
                        :scene/scene
                        {:fx/args
                         [(fx/make
                           :scene.layout/v-box
                           {:fx/args [(double 0)]
                            :background back
                            :children
                            [(fx/make
                              :scene.control/text-field
                              {:id     "prompt-text-field"
                               :style  "-fx-font-size: 2.5em;"
                               :text   "POPU22222!"
                               :effect drop-shadow
                               :border Border/EMPTY})
                             (fx/make
                              :scene.control/list-view
                              {:items (observable-list
                                       ["windows/split-below"
                                        "windows/split-right"
                                        "windows/maximize"
                                        "windows/delete"])})]})
                          nil]
                         ;;:fx/setup #(style/add-stylesheet % "css/default.css")
                         })]
                      [:fx/setup #(.show %)]])]
    (alter-var-root #'popup (constantly s))
    s))

(defn scenic-view [node]
  (ScenicView/show node))

(comment
  (fx/run-later! make-popup)
  (style/add-stylesheet (-> popup .getScene) "file:///tmp/default.css")
  (fx/run-later! #(scenic-view (-> popup .getScene)))
  (fx/run-later! #(.close popup))
  )

(defin execute-command
  {:alias :execute-command
   :params [{:type :text}]}
  [function-name]
  (let [alias (keyword function-name)]
    (if-let [{:keys [var]} (get datacore/interactive-functions alias)]
      (let [fun (deref var)]
        (fun))
      (message/error (str "No interactive function with alias " alias " found!")))))

;;to see live CSS updates:

(comment
  (add-stylesheet @scene "file:///tmp/default.css"))
