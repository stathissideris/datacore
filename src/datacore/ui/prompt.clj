(ns datacore.ui.prompt
  (:require [datacore.ui.java-fx :as fx]
            [datacore.ui.observable :refer [observable-list]]
            [datacore.ui.util :as uu]
            [datacore.ui.view :as view]
            [datacore.cells :as c]
            [datacore.ui.interactive :as in :refer [defin]])
  (:import [javafx.scene.control ListCell]))

(def state
  (atom {}))

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

(defn make-popup [{:keys [title prompt-text autocomplete-fn initial-input accept-fn cancel-fn]}]
  ;;.centerOnScreen
  ;;.setOpacity
  (let [autocomplete-list (atom [])
        prompt
        (fx/make-tree
         {:fx/type     :scene.layout/v-box
          :style-class ["focus-parent"]
          :dc/meta     {::view/type ::view/prompt}
          :style       (str "-fx-padding: 40px;"
                            "-fx-background-color: rgba(0,0,0,0);")
          :children
          [{:fx/type :scene.layout/v-box
            :style (str "-fx-padding: 0px;"
                        "-fx-background-color: rgba(0,0,0,0);"
                        "-fx-effect: dropshadow(gaussian, rgba(0,0,0,0.3), 20, 0, 0, 10)")
            :pref-width 400 ;;if not set, :wrap-text does not work on label below
            :children
            [{:fx/type     :scene.text/text-flow
              :fx/children [[:span {:fill (-> "#887373" fx/color fx/darker) :size 16} title]
                            "\n"
                            [:span {:fill (fx/color "#887373")} prompt-text]]
              :style       (str "-fx-padding: 0.9em 0.7em 0.6em 0.8em;"
                                "-fx-background-color: #f4f2f3;"
                                "-fx-background-radius: 8 8 0 0;"
                                "-fx-border-width: 1 1 0 1;"
                                "-fx-border-radius: 6 6 0 0;"
                                "-fx-border-color: #d5d5d5;"
                                "-fx-wrap-text: true;")}
             {:fx/type          :scene.control/text-field
              :id               "input"
              :style            (str "-fx-font-size: 2.5em;"
                                     "-fx-border-width: 5px;"
                                     "-fx-background-color: #f4f2f3;"
                                     "-fx-border-color: #d5d5d5;"
                                     "-fx-border-style: solid;"
                                     "-fx-border-width: 1px;")
              :text             initial-input
              :fx/prop-listener [:text
                                 (fn [_ _ _ text]
                                   (when autocomplete-fn
                                     (future
                                       (reset! autocomplete-list (autocomplete-fn text)))))]}
             (when autocomplete-fn
               {:fx/type      :scene.control/list-view
                :id           "autocomplete-list"
                :style        (str "-fx-font-size: 1.5em;"
                                   "-fx-border-color: white;"
                                   "-fx-border-style: solid;"
                                   "-fx-border-width: 1px;"
                                   "-fx-border-radius: 0 0 5 5;"
                                   "-fx-background-radius: 0 0 2 2;")
                :items        (observable-list
                               (if autocomplete-fn
                                 (autocomplete-fn initial-input)
                                 []))
                :cell-factory (fx/callback
                               (fn [list]
                                 (list-cell)))})]}]})
        window            (view/build-view
                           {:type         :datacore.ui.view/window
                            :raw-root     prompt
                            :window-style :transparent})]
    (add-watch
     autocomplete-list :autocomplete
     (fn [_ _ _ new]
       (let [list  (fx/find-by-id prompt "autocomplete-list")
             items (observable-list (vec new))]
         (fx/run-later!
          #(do
             (fx/set-field! list :items items)
             (-> list .getSelectionModel .selectFirst))))))
    (reset! state {:accept-fn accept-fn
                   :cancel-fn cancel-fn})
    (in/call :prompt/end)
    window))

;;;;;;;;;;;;;;;;;;;; interactive ;;;;;;;;;;;;;;;;;;;;

(def ^:private scroll-to* (-> javafx.scene.control.ListView (.getMethod "scrollTo" (into-array [Integer/TYPE]))))

(defn- scroll-to [list index]
  (fx/run-later!
   #(.invoke scroll-to* list (object-array [(int index)]))))

(defn- move-suggestion
  [component direction]
  (let [input-box      (fx/find-by-id component "input")
        list           (fx/find-by-id component "autocomplete-list")
        length         (some-> list .getItems .size)
        selection      (.getSelectionModel list)
        selected       (.getSelectedIndex selection)
        new-selected   (+ selected (if (= :next direction) 1 -1))
        new-selected   (cond (= -1 new-selected) (dec length)
                             (= length new-selected) 0
                             :else new-selected)
        [first-visible
         last-visible] (fx/get-field list :fx/visible-range)]
    (.clearAndSelect selection new-selected)
    (when (or (< new-selected first-visible)
              (>= new-selected last-visible))
      (scroll-to list new-selected))
    (.requestFocus input-box)))

(defin next-suggestion
  {:alias :prompt/next-suggestion
   :params [[:component ::in/focus-parent]]}
  [{:keys [component]}]
  (move-suggestion component :next))

(defin prev-suggestion
  {:alias :prompt/prev-suggestion
   :params [[:component ::in/focus-parent]]}
  [{:keys [component]}]
  (move-suggestion component :prev))

(defin home
  {:alias :prompt/home
   :params [[:component ::in/focus-parent]]}
  [{:keys [component]}]
  (.home (fx/find-by-id component "input")))

(defin end
  {:alias :prompt/end
   :params [[:component ::in/focus-parent]]}
  [{:keys [component]}]
  (when component
   (let [input (fx/find-by-id component "input")]
     (when input
       (fx/run-later! #(doto input
                         (.requestFocus)
                         (.end)))))))

(defin backward-char
  {:alias :prompt/backward-char
   :params [[:component ::in/focus-parent]]}
  [{:keys [component]}]
  (.backward (fx/find-by-id component "input")))

(defin forward-char
  {:alias :prompt/forward-char
   :params [[:component ::in/focus-parent]]}
  [{:keys [component]}]
  (.forward (fx/find-by-id component "input")))

(defin delete-forward-char
  {:alias :prompt/delete-forward-char
   :params [[:component ::in/focus-parent]]}
  [{:keys [component]}]
  (.deleteNextChar (fx/find-by-id component "input")))

(defin cancel
  {:alias :prompt/cancel
   :params [[:component ::in/focus-parent]]}
  [{:keys [component]}]
  (when-let [stage (fx/stage-of component)]
    (fx/run-later! #(.close stage)))
  (let [cancel-fn (:cancel-fn @state)]
    (reset! state {})
    (when cancel-fn (cancel-fn))))

(defin complete
  {:alias :prompt/complete
   :params [[:component ::in/focus-parent]]}
  [{:keys [component]}]
  (let [input-box (fx/find-by-id component "input")
        list      (fx/find-by-id component "autocomplete-list")
        selection (.getSelectionModel list)
        length    (some-> list .getItems .size)
        selected  (or (.getSelectedItem selection)
                      (when (= 1 length) (some-> list .getItems first)))]
    (when selected
      (.setText input-box (:raw selected))
      (in/call :prompt/end))))

(defin accept
  {:alias :prompt/accept
   :params [[:component ::in/focus-parent]]}
  [{:keys [component]}]
  (let [input-box (fx/find-by-id component "input")
        list      (fx/find-by-id component "autocomplete-list")
        selection (.getSelectionModel list)
        length    (some-> list .getItems .size)
        selected  (or (.getSelectedItem selection)
                      (when (= 1 length) (some-> list .getItems first)))
        accept-fn (:accept-fn @state)
        stage     (fx/stage-of component)]
    (when stage (fx/run-later! #(.close stage)))
    (when accept-fn (accept-fn {:input-text    (.getText input-box)
                                :selected-item (:value selected)}))))

(defmethod in/resolve-param ::in/function
  [{:keys [title prompt initial-input]}]
  (let [out (promise)]
    (fx/run-later!
     #(-> (make-popup
           {:title           "execute-function"
            :prompt-text     (str "Select the function to execute")
            :autocomplete-fn in/function-autocomplete
            :initial-input   "w" ;;TODO if you make this an empty string the JVM crashes!!!!
            :accept-fn       (fn [selected]
                               (prn 'SELECTED selected)
                               (deliver out (:selected-item selected)))})
          fx/show!))
    @out))

(comment
  (do
    (c/defcell popup-preview (make-popup))
    (uu/inspect popup-preview))
  )

(comment
  (fx/run-later!
   #(-> (make-popup
         {:title           "execute-function"
          :prompt-text     (str "Select the function to execute")
          :autocomplete-fn in/function-autocomplete
          :initial-input   "wind"
          :accept-fn       (fn [text] (prn 'SELECTED text))})
        fx/show!))
  )

(comment
  (fx/run-later!
   #(-> (make-popup
         {:autocomplete-fn in/file-autocomplete
          :initial-text    "/"})
        fx/show!))
  )

(comment
  (fx/run-later!
   #(-> (make-popup
         {:initial-text "foo"})
        fx/show!))
  )
