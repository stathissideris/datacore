(ns datacore.ui
  (:require [datacore.ui.keys :as keys]
            [datacore.ui.keys.defaults :as default-keys]
            [datacore.ui.style :as style]
            [datacore.ui.table :as table]
            [datacore.ui.message :as message]
            [datacore.ui.java-fx :as fx]
            [datacore.cells :refer [cell=]])
  (:import [javafx.embed.swing JFXPanel]
           [javafx.stage Stage]
           [javafx.application Platform]
           [javafx.scene.input KeyEvent]
           [javafx.event EventHandler Event]))

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

(defmacro doto-cond-> [x & clauses]
  (let [comp (gensym)]
    `(let [~comp ~x]
       ~@(for [[pred code] (partition 2 clauses)]
           `(when ~pred (-> ~comp ~code)))
       ~comp)))

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
  (fx/make :scene.control/label {:text "status"}))

(defn with-status-line [c]
  (fx/make
   :scene.layout/border-pane
   {:center c
    :bottom (status-line)}))

(defmulti build-view :type)

(defmethod build-view :split-pane
  [{:keys [orientation children]}]
  (fx/make :scene.control/split-pane
           {:items       (map build-view children)
            :orientation (if (= orientation :horizontal)
                           javafx.geometry.Orientation/HORIZONTAL
                           javafx.geometry.Orientation/VERTICAL)}))

(defmethod build-view :text-area
  [{:keys [text]}]
  (with-status-line
    (fx/make :scene.control/text-area {:fx/args ["MINIBUFFER"]})))

(defmethod build-view :table
  [{:keys [data]}]
  (with-status-line
    (table/set-columns!
     (table/view data)
     [(table/column "foo" :a)
      (table/column "bar" :b)
      (table/column "baz" :c)])))

(defn main-view [panes message]
  (let [minibuffer (fx/make :scene.control/text-area {:fx/args ["MINIBUFFER"]})]
    (cell= (fx/set-field! minibuffer :text @message))
    (fx/make
     :scene.layout/border-pane
     {:center (build-view panes)
      :bottom minibuffer})))

(def scene (atom nil))
(defn make-app []
  (let [the-scene   (fx/make :scene/scene
                             {:fx/args  [(main-view panes message/current-message) 800 800]
                              :fx/setup #(style/add-stylesheet % "css/default.css")})
        key-handler (keys/key-handler default-keys/root-keymap)]
    (reset! scene the-scene)
    (comment
      (fx/make
       :stage/stage
       {:scene    the-scene
        :title    "foobar"
        :fx/setup #(.addEventFilter
                    % KeyEvent/ANY
                    (reify EventHandler
                      (^void handle [this ^Event event]
                       (key-handler event))))}))
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
  (do (dev/refresh) (datacore.ui/run-later! datacore.ui/make-app))
  )

;;to see the table being updated live:

(comment
  (swap! table-data assoc-in [0 :b] 10000)
  (swap! table-data assoc-in [2 :a] "fooo")
  (swap! table-data conj {:a (rand-int 100), :b (rand-int 100), :c (rand-int 100)}))

;;to see live CSS updates:

(comment
  (add-stylesheet @scene "file:///tmp/default.css"))
