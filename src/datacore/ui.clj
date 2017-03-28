(ns datacore.ui
  (:require [datacore :refer [defin]]
            [datacore.ui.keys :as keys]
            [datacore.ui.keys.defaults :as default-keys]
            [datacore.ui.style :as style]
            [datacore.ui.message :as message]
            [datacore.view :as view]
            [datacore.ui.java-fx :as fx]
            [datacore.ui.util :as uu]
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
            :style     "-fx-padding: 0.6em 0.6em 0.6em 0.6em;"
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

#_(fx/make :scene.effect/drop-shadow
           {:radius 5.0
            :offset-y 3.0})


(defn make-popup []
  ;;.centerOnScreen
  ;;.setOpacity
  (fx/make
   :scene.layout/v-box
   {:style (str "-fx-padding: 40px;"
                "-fx-background-color: rgba(0,0,0,0);")
    :children
    [(fx/make
      :scene.layout/v-box
      {:style (str "-fx-padding: 0px;"
                   "-fx-background-color: rgba(0,0,0,0);"
                   "-fx-effect: dropshadow(gaussian, rgba(0,0,0,0.3), 20, 0, 0, 10)")
       :pref-width 400 ;;if not set, :wrap-text does not work on label below
       :children
       [(fx/make :scene.control/label
                 {:text      (str "execute-function\n"
                                  "Function to execute"
                                  " (this is a pretty long label to test the wrapping of label, I need a bit more text):")
                  :wrap-text true
                  :style     (str "-fx-padding: 0.9em 0.7em 0.6em 0.8em;"
                                  "-fx-font-size: 0.85em;"
                                  "-fx-background-color: #f4f2f3;"
                                  "-fx-background-radius: 8 8 0 0;"
                                  "-fx-border-width: 1 1 0 1;"
                                  "-fx-border-radius: 6 6 0 0;"
                                  "-fx-border-color: #d5d5d5;"
                                  "-fx-wrap-text: true;"
                                  "-fx-text-fill: #887373;")})
        (fx/make
         :scene.control/text-field
         {:id     "prompt-text-field"
          :style  (str "-fx-font-size: 2.5em;"
                       "-fx-border-width: 5px;"
                       "-fx-background-color: #f4f2f3;"
                       "-fx-border-color: #d5d5d5;"
                       "-fx-border-style: solid;"
                       "-fx-border-width: 1px;")
          :text   "wind"
          :border Border/EMPTY})
        (fx/make
         :scene.control/list-view
         {:style (str "-fx-font-size: 1.5em;"
                      "-fx-border-color: white;"
                      "-fx-border-style: solid;"
                      "-fx-border-width: 1px;"
                      "-fx-border-radius: 0 0 5 5;"
                      "-fx-background-radius: 0 0 2 2;")
          :items (observable-list
                  ["windows/split-below"
                   "windows/split-right"
                   "windows/maximize"
                   "windows/delete"])})]})]}))

(comment
  (do
    (c/defcell popup-preview (make-popup))
    (uu/inspect popup-preview))
  )
;;(def cc (fx/run-later! #(-> (make-popup) fx/transparent-window fx/show)))
;;(fx/run-later! #(-> cc deref .close))
;;(c/swap! popup-preview (fn [_] (make-popup)))

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
