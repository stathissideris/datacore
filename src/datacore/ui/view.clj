(ns datacore.ui.view
  (:require [datacore.ui.java-fx :as fx]
            [datacore.util :as util]
            [datacore.cells :as c]
            [datacore.ui.keys :as keys]
            [datacore.ui.keys.defaults :as default-keys]
            [datacore.ui.message :as message]
            [datacore.state :as state])
  (:import [javafx.scene.input KeyEvent]
           [javafx.event EventHandler Event]
           [javafx.scene.paint Color]))

(defn- register-component! [view-id fx-component]
  (c/swap! state/view-to-component assoc view-id fx-component))

;;TODO destroy view (also destroy FX component)

(defmulti build-view (fn [x] (or (:type x)
                                 (when (c/cell-id? x) (:type (c/value x))))))

(defn- component-id [c]
  (if (c/cell-id? c)
    c
    (:id c)))

(defmethod build-view ::nothing
  [_]
  (fx/make
   :scene.layout/border-pane
   {:center (fx/label "Nothing to show")}))

(defmethod build-view ::vector [v] (mapv build-view v))

(defmethod build-view ::split-pane
  [{:keys [orientation children]}]
  (let [components (map build-view children)]
    (doall
     (map
      (fn [spec component]
        (register-component! (component-id spec) component))
      children
      components))
    (fx/make :scene.control/split-pane
             {:items       components
              :orientation (if (= orientation :horizontal)
                             javafx.geometry.Orientation/HORIZONTAL
                             javafx.geometry.Orientation/VERTICAL)})))

(defmethod build-view ::existing
  [{:keys [component view]}]
  (if component
    component
    (fx/make :scene.control/label
             {:text (str "ERROR! component for view " view " does not exist")})))

(defn- message-line [message]
  (fx/make :scene.control/label
           {:text      (c/formula :msg message)
            :style     "-fx-padding: 0.6em 0.6em 0.6em 0.6em;"
            :text-fill (c/formula (comp {:message Color/BLACK
                                         :error   (Color/web "0xF57000")}
                                        :type) message)}))

(defn build-window-contents [tree message]
  (fx/make
   :scene.layout/border-pane
   {:center (if-not tree
              (build-view ::nothing)
              (build-view tree))
    :bottom (message-line message)}))

;;TODO add this to scene: :fx/setup #(style/add-stylesheet % "css/default.css")
(defmethod build-view ::window
  [{:keys [title dimensions root]}]
  (let [[width height] dimensions
        key-handler    (keys/key-handler default-keys/root-keymap)
        scene          (fx/make :scene/scene {:fx/args [(build-window-contents root message/current-message) width height]})]
    (fx/make
     :stage/stage
     [[:title title]
      [:scene scene]
      [:fx/setup #(doto %
                    (.addEventFilter
                     KeyEvent/ANY
                     (reify EventHandler
                       (^void handle [this ^Event event]
                        (key-handler event)))))]])))

;;;;;;;;;;;;;;;;

(require '[clojure.pprint :as pp])
(defn update-layout! [old-tree new-tree]
  (let [old-windows (:children old-tree)
        new-windows (:children new-tree)
        diffs       (util/seq-diff old-windows new-windows)]
    (doseq [[type {:keys [id] :as window-spec} :as diff] diffs]
      (condp = type
        :same   :skip
        :add    (let [component @(fx/run-later! #(build-view window-spec))
                      id        (:id window-spec)]
                  (register-component! id component)
                  (fx/run-later! #(fx/show component)))
        :delete (let [component (get (c/value state/view-to-component) id)]
                  (when component
                    ;;TODO unregister component
                    (fx/run-later! #(.close component))))
        :edit   (let [[_ old-window new-window] diff]
                  (when-not (= (:root old-window) (:root new-window))
                    (let [stage-component (get (c/value state/view-to-component) id)
                          scene           (.getScene stage-component)]
                      (fx/run-later!
                       #(.setRoot scene (build-window-contents (:root new-window) message/current-message))))))))))
