(ns datacore.ui.prompt
  (:require [datacore.ui.java-fx :as fx]
            [datacore.ui.observable :refer [observable-list]]
            [datacore.ui.util :as uu]
            [datacore.ui.view :as view]
            [datacore.cells :as c]
            [datacore.ui.interactive :as i]
            [clojure.string :as str]))

(defn make-popup [{:keys [autocomplete-fun]}]
  ;;.centerOnScreen
  ;;.setOpacity
  (let [autocomplete-list (atom [])]
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
          :text             "wind"
          :fx/prop-listener [:text
                             (fn [_ _ text]
                               (when autocomplete-fun
                                 (reset! autocomplete-list (autocomplete-fun text))))]}
         {:fx/type :scene.control/list-view
          :style   (str "-fx-font-size: 1.5em;"
                        "-fx-border-color: white;"
                        "-fx-border-style: solid;"
                        "-fx-border-width: 1px;"
                        "-fx-border-radius: 0 0 5 5;"
                        "-fx-background-radius: 0 0 2 2;")
          :items   (observable-list autocomplete-list)}]}]})))

(defmethod view/build-view ::view/prompt
  [_]
  (make-popup))

(comment
  (do
    (c/defcell popup-preview (make-popup))
    (uu/inspect popup-preview))
  )

(defn function-autocomplete [input]
  (if (empty? (str/trim input))
    (->> i/functions
         keys
         (map #(-> % str (subs 1)))
         sort
         (take 50))
    (->> i/functions
         keys
         (map #(-> % str (subs 1)))
         (filter #(str/includes? % input))
         sort)))

;;(def cc (deref (fx/run-later! #(-> (make-popup {:autocomplete-fun function-autocomplete}) fx/transparent-window fx/show!))))
;;(fx/run-later! #(-> cc .close))
;;(c/swap! popup-preview (fn [_] (make-popup)))
