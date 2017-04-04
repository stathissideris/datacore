(ns datacore.ui.view
  (:require [datacore.ui.java-fx :as fx]
            [datacore.util :as util]
            [datacore.cells :as c]
            [datacore.ui.keys :as keys]
            [datacore.ui.keys.defaults :as default-keys]
            [datacore.ui.message :as message]
            [datacore.state :as state])
  (:import [javafx.scene.input KeyEvent]
           [javafx.scene.paint Color]
           [javafx.stage StageStyle]))

(defn- register-component! [view-id fx-component]
  (c/swap! state/view-to-component assoc view-id fx-component)
  fx-component)

(defn- unregister-component! [view-id]
  (c/swap! state/view-to-component dissoc view-id))

(defmulti build-view (fn [x] (or (:type x)
                                 (when (c/cell-id? x) (:type (c/value x))))))

(defmethod build-view ::nothing
  [{:keys [id]}]
  (let [component (fx/make
                   :scene.layout/border-pane
                   {:id          id
                    :style-class ["focusable"]
                    :center      (fx/label "Nothing to show")})]
    (register-component! id component)))

(defmethod build-view ::split-pane
  [{:keys [orientation children]}]
  (let [components (map build-view children)]
    (doall
     (map
      (fn [spec component]
        (register-component! (:id spec) component))
      children
      components))
    (fx/make :scene.control/split-pane
             {:items       components
              :orientation (if (= orientation :horizontal)
                             javafx.geometry.Orientation/HORIZONTAL
                             javafx.geometry.Orientation/VERTICAL)})))

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

(def window-style-map
  {:normal      StageStyle/DECORATED
   :undecorated StageStyle/UNDECORATED
   :transparent StageStyle/TRANSPARENT})

(defn close-window! [component-id]
  (state/swap-layout! update :children (fn [c] (remove #(= component-id (:id %)) c))))

;;TODO add this to scene: [:fx/setup #(style/add-stylesheet % "css/default.css")]
(defmethod build-view ::window
  [{:keys [id title dimensions root window-style]}]
  (let [[width height] dimensions
        key-handler    (keys/key-handler default-keys/root-keymap)
        scene-args     (concat
                        [(if-not (or (nil? window-style) (= :normal window-style))
                           (build-view (or root ::nothing))
                           (build-window-contents root message/current-message))]
                        (when dimensions [width height]))
        scene          (fx/make :scene/scene
                                [[:fx/args scene-args]
                                 (when (= window-style :transparent)
                                   [:fill Color/TRANSPARENT])])
        stage          (fx/make
                        :stage/stage
                        [(when window-style
                           [:fx/args [(get window-style-map window-style)]])
                         (when title [:title title])
                         [:scene scene]
                         [:on-close-request (fx/event-handler (fn [event]
                                                                (.consume event)
                                                                (close-window! id)))]
                         [:fx/setup #(doto %
                                       (.addEventFilter
                                        KeyEvent/ANY
                                        (fx/event-handler key-handler)))]])]
    (-> scene
        .focusOwnerProperty
        (.addListener
         (fx/change-listener
          (fn [_ old new]
            (when-let [component-id (some->> (cons new (fx/parents new))
                                             (filter #(fx/has-style-class? % "focusable"))
                                             first
                                             .getId)]
              (c/swap! state/window->focused-component assoc id component-id)
              (c/reset! state/focus component-id))
            (println "COMPONENT FOCUSED:" new)))))
    (-> stage
        .focusedProperty
        (.addListener
         (fx/change-listener
          (fn [_ _ new]
            (when new
              (when-let [component-id (-> state/window->focused-component c/value (get id))]
                (c/reset! state/focus component-id))
              (println "WINDOW FOCUSED:" id title))))))
    stage))

(defmethod build-view ::cell
  [{:keys [id cell]}]
  (let [component (build-view cell)]
    (fx/set-fields! component {:id          id
                               :style-class ["focusable"]}) ;;TODO add style-class instead of replacing the whole list
    (register-component! id component)
    component))

;;;;;;;;;;;;;;;;

(defn update-layout! [old-tree new-tree]
  (let [old-windows (:children old-tree)
        new-windows (:children new-tree)
        diffs       (util/seq-diff old-windows new-windows)]
    (doseq [[type {:keys [id] :as window-spec} :as diff] diffs]
      (condp = type
        :same   :skip
        :insert (let [component @(fx/run-later! #(build-view window-spec))]
                  (register-component! id component)
                  (fx/run-later! #(fx/show component)))
        :delete (let [component (get (c/value state/view-to-component) id)]
                  (when component
                    (unregister-component! id)
                    (fx/run-later! #(.close component))))
        :edit   (let [[_ old-window new-window] diff]
                  (when-not (= (:root old-window) (:root new-window))
                    (let [stage-component (get (c/value state/view-to-component) id)
                          scene           (.getScene stage-component)]
                      (fx/run-later!
                       #(.setRoot scene (build-window-contents (:root new-window) message/current-message))))))))))
