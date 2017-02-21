(ns datacore.ui
  (:require [datacore.ui.keys :as keys]
            [datacore.ui.keys.defaults :as default-keys]
            [datacore.ui.style :as style]
            [datacore.ui.table :as table])
  (:import [javafx.embed.swing JFXPanel]
           [javafx.application Application]
           [javafx.scene Group Scene]
           [javafx.scene.shape Circle]
           [javafx.stage Stage Modality]
           [javafx.application Platform]
           [javafx.scene.layout Pane HBox BorderPane]
           [javafx.scene.control Control Button SplitPane TextArea Label]
           [javafx.collections ObservableList]
           [javafx.scene.input KeyEvent]
           [javafx.event EventHandler Event]
           [java.util Collection]))

(def table-data
  (atom [{:a 6 :b 7 :c 8}
         {:a 16 :b 17 :c 18}
         {:a 26 :b 27 :c 28}]))

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
                  :children    [{:type :table
                                 :data table-data}
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

(defmethod build-view :table
  [{:keys [data]}]
  (with-status-line
    (table/set-columns!
     (table/view data)
     [(table/column "foo" :a)
      (table/column "bar" :b)
      (table/column "baz" :c)])))

(defn main-view [panes]
  (border-pane
   {:center
    (build-view panes)
    :bottom (TextArea. "MINIBUFFER")}))

(def scene (atom nil))
(defn make-app []
  (let [the-scene   (doto (Scene. (main-view panes) 800 800)
                      (style/add-stylesheet "css/default.css"))
        key-handler (keys/key-handler default-keys/root-keymap)]
    (reset! scene the-scene)
    (doto (Stage.)
      (.setScene the-scene)
      (.setTitle "foobar")
      (.addEventFilter
       KeyEvent/ANY
       (reify EventHandler
         (^void handle [this ^Event event]
          (key-handler event))))
      (.show))))

(comment
  (run-later! make-app)
  )

;;to see the table being updated live:

(comment
  (swap! table-data assoc-in [0 :b] 10000)
  (swap! table-data assoc-in [2 :a] "fooo")
  (swap! table-data conj {:a (rand-int 100), :b (rand-int 100), :c (rand-int 100)}))

;;to see live CSS updates:

(comment
  (add-stylesheet @scene "file:///tmp/default.css"))
