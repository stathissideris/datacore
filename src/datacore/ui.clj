(ns datacore.ui
  (:require [datacore.ui.keys :as keys])
  (:import [javafx.embed.swing JFXPanel]
           [javafx.application Application]
           [javafx.scene Group Scene]
           [javafx.scene.shape Circle]
           [javafx.stage Stage Modality]
           [javafx.application Platform]
           [javafx.scene.layout Pane HBox BorderPane]
           [javafx.scene.control Button SplitPane TextArea Label]
           [javafx.collections ObservableList]
           [javafx.scene.input KeyEvent]
           [javafx.event EventHandler Event]
           [java.util Collection]))

;;(set! *warn-on-reflection* true)
(JFXPanel.)
(Platform/setImplicitExit false)

(defn run-later! [fun]
  (if (Platform/isFxApplicationThread)
    (fun)
    (Platform/runLater
     (fn []
       (try
         (fun)
         (catch Exception e
           (.printStackTrace e)
           (throw e)))))))

(defn set-children! [^Pane c coll]
  (.setAll ^ObservableList (.getChildren c) ^Collection coll))

(defn button [text]
  (doto (Button.)
    (.setText text)))

(defn test-pane []
  (doto (HBox. (double 8))
    (set-children!
     [(button "foo")])))

(defn split-pane
  ([panes]
   (split-pane nil panes))
  ([orientation panes]
   (let [pane (SplitPane.)]
     (.setAll ^ObservableList (.getItems pane) ^Collection panes)
     (when orientation
       (condp = orientation
         :horizontal (.setOrientation pane javafx.geometry.Orientation/HORIZONTAL)
         :vertical (.setOrientation pane javafx.geometry.Orientation/VERTICAL)))
     pane)))

(defmacro doto-cond-> [x & clauses]
  (let [comp (gensym)]
    `(let [~comp ~x]
       ~@(for [[pred code] (partition 2 clauses)]
           `(when ~pred (-> ~comp ~code)))
       ~comp)))

(defn border-pane [{:keys [top bottom left right center] :as mm}]
  (let [b (BorderPane.)]
    (doto-cond-> b
      top (.setTop top)
      bottom (.setBottom bottom)
      center (.setCenter center)
      left (.setLeft left)
      right (.setRight right))))

(def panes
  {:type        :split-pane
   :orientation :horizontal
   :children    [{:type        :split-pane
                  :orientation :vertical
                  :children    [{:type :text-area
                                 :text "FOO A"}
                                {:type :text-area
                                 :text "FOO B"}]}
                 {:type        :split-pane
                  :orientation :vertical
                  :children    [{:type :text-area
                                 :text "FOO C"}
                                {:type :text-area
                                 :text "FOO D"}]}]})

(defn status-line []
  (Label. "status"))

(defn with-status-line [c]
  (border-pane
   {:center c
    :bottom (status-line)}))

(defmulti build-view :type)

(defmethod build-view :split-pane
  [{:keys [orientation children]}]
  (split-pane orientation (map build-view children)))

(defmethod build-view :text-area
  [{:keys [text]}]
  (with-status-line
    (TextArea. text)))

(defn main-view [panes]
  (let [view (border-pane
              {:center
               (build-view panes)
               :bottom (TextArea. "MINIBUFFER")})]
    (doto view
      (.setStyle "-fx-base: rgb(30, 30, 35);"))))

(defn make-app []
  (let [scene (Scene. (main-view panes) 800 800)]
    (doto (Stage.)
      (.setScene scene)
      (.setTitle "foobar")
      (.addEventFilter
       KeyEvent/ANY
       (reify EventHandler
         (^void handle [this ^Event event]
          (keys/global-key-handler event))))
      (.show))))

(comment
  (run-later! make-app)
  )
