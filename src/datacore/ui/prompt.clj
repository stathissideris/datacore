(ns datacore.ui.prompt
  (:require [datacore.ui.java-fx :as fx]
            [datacore.ui.observable :refer [observable-list]]
            [datacore.ui.util :as uu]
            [datacore.ui.view :as view]
            [datacore.cells :as c]
            [datacore.ui.interactive :as i])
  (:import [javafx.scene.control ListCell]))

(defn list-cell []
  (let [tf (fx/text-flow)]
    (proxy [ListCell] []
      (updateItem
        ([item empty]
         (proxy-super updateItem item empty)
         (.setText this nil)
         (if empty
           (.setGraphic this nil)
           (do
             (-> tf .getChildren (.setAll (map fx/text (:text item))))
             (.setGraphic this tf))))))))

(defn make-popup [{:keys [autocomplete-fun initial-text]}]
  ;;.centerOnScreen
  ;;.setOpacity
  (let [autocomplete-list (atom [])
        prompt
        (fx/make-tree
         {:fx/type  :scene.layout/v-box
          :style    (str "-fx-padding: 40px;"
                         "-fx-background-color: rgba(0,0,0,0);")
          :children
          [{:fx/type :scene.layout/v-box
            :style (str "-fx-padding: 0px;"
                        "-fx-background-color: rgba(0,0,0,0);"
                        "-fx-effect: dropshadow(gaussian, rgba(0,0,0,0.3), 20, 0, 0, 10)")
            :pref-width 400 ;;if not set, :wrap-text does not work on label below
            :children
            [{:fx/type   :scene.control/label
              :text      (str "execute-function\n"
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
                              "-fx-text-fill: #887373;")}
             {:fx/type          :scene.control/text-field
              :id               "prompt-text-field"
              :style            (str "-fx-font-size: 2.5em;"
                                     "-fx-border-width: 5px;"
                                     "-fx-background-color: #f4f2f3;"
                                     "-fx-border-color: #d5d5d5;"
                                     "-fx-border-style: solid;"
                                     "-fx-border-width: 1px;")
              :text             initial-text
              :fx/prop-listener [:text
                                 (fn [_ _ text]
                                   (when autocomplete-fun
                                     (future
                                       (reset! autocomplete-list (autocomplete-fun text)))))]}
             {:fx/type      :scene.control/list-view
              :id           "autocomplete-list"
              :style        (str "-fx-font-size: 1.5em;"
                                 "-fx-border-color: white;"
                                 "-fx-border-style: solid;"
                                 "-fx-border-width: 1px;"
                                 "-fx-border-radius: 0 0 5 5;"
                                 "-fx-background-radius: 0 0 2 2;")
              :items        (observable-list
                             (if autocomplete-fun
                               (autocomplete-fun initial-text)
                               []))
              :cell-factory (fx/callback
                             (fn [list]
                               (list-cell)))}]}]})]
    (add-watch
     autocomplete-list :autocomplete
     (fn [_ _ _ new]
       (let [list  (fx/find-by-id prompt "autocomplete-list")
             items (observable-list (vec new))]
         (fx/run-later!
          #(fx/set-field! list :items items)))))
    (view/build-view
     {:type         :datacore.ui.view/window
      :raw-root     prompt
      :window-style :transparent})))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(comment
  (do
    (c/defcell popup-preview (make-popup))
    (uu/inspect popup-preview))
  )

(comment
  (def cc (deref (fx/run-later!
                  #(-> (make-popup
                        {:autocomplete-fun i/function-autocomplete
                         :initial-text     "wind"})
                       fx/show!))))
  )

(comment
  (def cc (deref (fx/run-later!
                  #(-> (make-popup
                        {:autocomplete-fun i/file-autocomplete
                         :initial-text     "/"})
                       fx/show!))))
  )

;;(fx/run-later! #(-> cc .close))
;;(c/swap! popup-preview (fn [_] (make-popup)))
