(ns datacore.ui.prompt
  (:require [datacore.ui.java-fx :as fx]
            [datacore.ui.observable :refer [observable-list]]
            [datacore.ui.util :as uu]
            [datacore.ui.view :as view]
            [datacore.cells :as c]
            [datacore.ui.interactive :as in :refer [defin]]
            [datacore.util :as util]
            [clojure.string :as str]
            [clojure.spec :as s]
            [me.raynes.fs :as fs])
  (:import [javafx.scene.control ListCell]))

(def state (atom nil))

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
             (let [texts (map fx/text (:text item))]
               (doseq [text texts]
                 (-> text .getStyleClass (.add "prompt-text")))
               (-> tf .getChildren (.setAll texts))
               (.setGraphic this tf)))))))))

(s/def ::title string?)
(s/def ::prompt-text string?)
(s/def ::input-strategy #{:free :constrained-autocomplete :free-autocomplete})
(s/def ::initial-input string?)

(s/def ::text (s/or :raw-string string? :formatted-text vector?))
(s/def ::raw string?)
(s/def ::value any?)
(s/def ::choice-item (s/keys :req-un [::text ::raw ::value]))
(s/def ::autocomplete-fn (s/fspec :args (s/cat :input string?)
                                  :ret (s/coll-of ::choice-item)))
(s/def ::input-text string?)
(s/def ::selected-item ::choice-item)
(s/def ::prompt-result (s/keys :req-un [::input-text ::selected-item ::input-strategy]))
(s/def ::valid?-fn (s/fspec :args (s/cat :input ::prompt-result)
                            :ret (s/or :valid #{true} :error-message string?)))
(s/def ::accept-fn (s/fspec :args (s/cat :input ::prompt-result)))
(s/def ::cancel-fn (s/fspec :args (s/cat)))

#_(s/fdef make-popup
        :args (s/cat :options
                     (s/keys :req-un [::title ::prompt-text]
                             :opt-un [::input-strategy ::initial-input
                                      ::autocomplete-fn ::accept-fn ::cancel-fn ::valid?-fn])))
(defn make-popup [{:keys [title
                          prompt-text
                          input-strategy
                          initial-input
                          autocomplete-fn
                          accept-fn
                          valid?-fn
                          cancel-fn]}]
  ;;.centerOnScreen
  ;;.setOpacity
  (when-not @state
    (let [autocomplete-list (atom [])
          prompt
          (fx/make-tree
           {:fx/type     :scene.layout/v-box
            :style-class ["focus-parent" "prompt"]
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
                :id          "info-text"
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
                                       (util/future
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
          window            (view/window
                             {:raw-root       prompt
                              :window-style   :transparent
                              :always-on-top? true})]
      (add-watch
       autocomplete-list :autocomplete
       (fn [_ _ _ new]
         (let [list  (fx/find-by-id prompt "autocomplete-list")
               items (observable-list (vec new))]
           (fx/run-later!
            #(do
               (fx/set-field! list :items items)
               (-> list .getSelectionModel .selectFirst))))))
      (reset! state {:accept-fn      accept-fn
                     :cancel-fn      cancel-fn
                     :valid?-fn      valid?-fn
                     :input-strategy input-strategy})
      (in/call :prompt/end)
      window)))
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

(defn- alpha? [x]
  (some? (re-matches #"[A-Za-z]" (str x))))

(defn- kill-last-word* [s]
  (->> (partition-by alpha? s)
       butlast
       butlast
       (map #(apply str %))
       (apply str)))

(defin kill-last-word
  {:alias :prompt/kill-last-word
   :params [[:component ::in/focus-parent]]}
  [{:keys [component]}]
  (let [input (fx/find-by-id component "input")
        text  (kill-last-word* (.getText input))]
    (fx/run-later!
     #(doto input
        (.setText text)
        (.end)))))

(defin cancel
  {:alias :prompt/cancel
   :params [[:component ::in/focus-parent]]}
  [{:keys [component]}]
  (when-let [stage (fx/stage-of component)]
    (fx/run-later! #(.close stage)))
  (let [cancel-fn (:cancel-fn @state)]
    (reset! state nil)
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

(defn- get-autocomplete-selection [component]
  (when-let [list (fx/find-by-id component "autocomplete-list")]
    (let [selection (.getSelectionModel list)
          length    (some-> list .getItems .size)]
      (or (.getSelectedItem selection)
          (when (= 1 length) (some-> list .getItems first))))))

(defn- get-free-text [component]
  (when-let [input-box (fx/find-by-id component "input")]
    (.getText input-box)))

(defin accept
  {:alias :prompt/accept
   :params [[:component ::in/focus-parent]]}
  [{:keys [component]}]
  (let [prompt-result {:input-text     (get-free-text component)
                       :selected-item  (get-autocomplete-selection component)
                       :input-strategy (or (:input-strategy @state) :constrained-autocomplete)}
        accept        (:accept-fn @state)
        valid?        (:valid?-fn @state)
        validation    (if valid? (valid? prompt-result) true)
        stage         (fx/stage-of component)]
    (if (true? validation)
      (do
        @(fx/run-later! #(when stage (.close stage)))
        (reset! state nil)
        (when accept (accept prompt-result)))
      (let [txt (fx/find-by-id component "info-text")]
        (fx/run-later!
         #(doto (fx/get-field txt :children)
            (.add (fx/text "\n"))
            (.add (fx/text [:span {:fill (fx/color "#ff0000")} validation]))))))))

(defmethod in/resolve-param ::in/function
  [{:keys [title prompt initial-input]}]
  (let [out (promise)]
    (fx/run-later!
     #(-> (make-popup
           {:title           title
            :prompt-text     prompt
            :autocomplete-fn in/function-autocomplete
            :initial-input   "w" ;;TODO if you make this an empty string the JVM crashes!!!!
            :accept-fn       (fn [result]
                               (deliver out (-> result :selected-item :value)))})
          fx/show!))
    @out))
(defmethod in/resolve-param-help ::in/function
  [_] "You will be prompted to choose a function.")

(defmethod in/resolve-param ::in/string
  [{:keys [title prompt initial-input]}]
  (let [out (promise)]
    (fx/run-later!
     #(-> (make-popup
           {:title       title
            :prompt-text prompt
            :accept-fn   (fn [result]
                           (deliver out (:input-text result)))})
          fx/show!))
    @out))
(defmethod in/resolve-param-help ::in/function
  [_] "You will be prompted to enter some text.")

(defmethod in/resolve-param ::in/clojure-code
  [{:keys [title prompt initial-input]}]
  (let [out (promise)]
    (fx/run-later!
     #(-> (make-popup
           {:title       title
            :prompt-text prompt
            :valid?-fn   in/validate-clojure-code
            :accept-fn   (fn [result]
                           (deliver out (:input-text result)))})
          fx/show!))
    @out))
(defmethod in/resolve-param-help ::in/function
  [_] "You will be prompted to enter some Clojure code.")

(defmethod in/resolve-param ::in/file
  [{:keys [title prompt initial-input]}]
  (let [out (promise)]
    (fx/run-later!
     #(-> (make-popup
           {:title           title
            :prompt-text     prompt
            :autocomplete-fn in/file-autocomplete
            :initial-input   (str (fs/home) ;;TODO if you make this an empty string the JVM crashes!!!!
                                  "/devel/datacore/test-resources"
                                  java.io.File/separator)
            :valid?-fn       in/validate-file
            :accept-fn       (fn [result]
                               (deliver out (-> result :selected-item :value)))})
          fx/show!))
    @out))
(defmethod in/resolve-param-help ::in/function
  [_] "You will be prompted to choose a file.")

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
