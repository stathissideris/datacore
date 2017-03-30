(ns datacore.ui.prompt
  (:require [datacore.ui.java-fx :as fx]
            [datacore.ui.observable :refer [observable-list]]
            [datacore.ui.util :as uu]
            [datacore.ui.view :as view]
            [datacore.cells :as c]))

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
          :text   "wind"})
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

(defmethod view/build-view ::view/prompt
  [_]
  (make-popup))

(comment
  (do
    (c/defcell popup-preview (make-popup))
    (uu/inspect popup-preview))
  )
;;(def cc (fx/run-later! #(-> (make-popup) fx/transparent-window fx/show)))
;;(fx/run-later! #(-> cc deref .close))
;;(c/swap! popup-preview (fn [_] (make-popup)))
