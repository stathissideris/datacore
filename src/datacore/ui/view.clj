(ns datacore.ui.view
  (:require [clojure.core.match :as m :refer [match]]
            [clojure.zip :as zip]
            [clojure.spec :as s]
            [datacore.ui.java-fx :as fx]
            [datacore.util :as util]
            [datacore.cells :as c]
            [datacore.ui.keys :as keys]
            [datacore.ui.keys.defaults :as default-keys]
            [datacore.ui.message :as message]
            [datacore.state :as state]
            [clojure.walk :as walk])
  (:import [javafx.scene.input KeyEvent]
           [javafx.scene.paint Color]
           [javafx.stage StageStyle]))

(defn- register-component! [view-id fx-component]
  (c/swap! state/view-to-component assoc view-id fx-component)
  fx-component)
(s/fdef register-component!
  :args (s/cat :view-id string? :fx-component any?))

(defn- unregister-component! [view-id]
  (c/swap! state/view-to-component dissoc view-id))

(defmulti build-view (fn [x] (or (:type x)
                                 (when (c/cell-id? x) (:type (c/value x))))))

#_(defn- get-or-build-view [{:keys [id focused?] :as component-map}]
  (or (and id (-> state/view-to-component c/value (get id) (set-focus-border! focused?))) ;;because focus changes sometimes get lost in the diffs
      (build-view component-map)))

(defn- get-or-build-view [{:keys [id focused?] :as component-map}]
  (build-view component-map))

(defn label [text]
  {:fx/type :scene.control/label
   :text    text})

(defmethod build-view ::nothing
  [{:keys [id]}]
  (let [component {:fx/type     :scene.layout/border-pane
                   :id          id
                   :style-class ["focusable"]
                   :center      (label "Nothing to show")}]
    (register-component! id component)))

(defmethod build-view nil
  [_]
  {:fx/type     :scene.layout/border-pane
   :style-class ["focusable"]
   :center      (label "Nothing to show")})

(defn set-focus-border [component focused?]
  (assoc
   component
   :style
    (if focused?
      (str "-fx-border-width: 4 4 4 4;"
           "-fx-border-color: #155477;")
      (str "-fx-border-width: 0 0 0 0;"))))

(defmethod build-view ::split-pane
  [{:keys [orientation children]}]
  (let [components (map #(-> (get-or-build-view %)
                             (set-focus-border (:focused? %))) children)]
    (doall
     (map
      (fn [spec component]
        (register-component! (:id spec) component))
      children
      components))
    {:fx/type     :scene.control/split-pane
     :items       components
     :orientation (if (= orientation :horizontal)
                    javafx.geometry.Orientation/HORIZONTAL
                    javafx.geometry.Orientation/VERTICAL)}))

(defn- message-line [message]
  {:fx/type   :scene.control/label
   :text      (c/formula :msg message)
   :style     "-fx-padding: 0.6em 0.6em 0.6em 0.6em;"
   :text-fill (c/formula (comp {:message Color/BLACK
                                :error   (Color/web "0xF57000")}
                               :type) message)})

(defn build-window-contents [{:keys [focused?] :as tree} message]
  {:fx/type :scene.layout/border-pane
   :center  (if-not tree
              (build-view ::nothing)
              (get-or-build-view tree))
   :bottom  (message-line message)})

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
                        [(build-view ::nothing)]
                        (when dimensions [width height]))
        scene-focus-l  (fx/change-listener
                        (fn [_ old new]
                          (when-let [component-id (when new
                                                    (some->> (cons new (fx/parents new))
                                                             (filter #(fx/has-style-class? % "focusable"))
                                                             first
                                                             .getId))]
                            (c/swap! state/window->focused-component assoc id component-id)
                            (c/reset! state/focus component-id))
                          (println "COMPONENT FOCUSED:" new)))
        stage-focus-l  (fx/change-listener
                        (fn [_ _ new]
                          (when new
                            (when-let [component-id (-> state/window->focused-component c/value (get id))]
                              (c/reset! state/focus component-id))
                            (println "WINDOW FOCUSED:" id title))))]
    (merge
     (when window-style
       {:fx/args [(get window-style-map window-style)]})
     (when title
       {:title title})
     {:fx/type          :stage/stage
      :scene            (merge
                         {:fx/type  :scene/scene
                          :fx/args  scene-args
                          :root     (build-window-contents root message/current-message)
                          :fx/setup #(do
                                       (-> % .focusOwnerProperty (.addListener scene-focus-l))
                                       %)}
                         (when (= window-style :transparent)
                           {:fill Color/TRANSPARENT}))
      :on-close-request (fx/event-handler (fn [event]
                                            (.consume event)
                                            (close-window! id)))
      :fx/setup         #(do
                           (.addEventFilter
                            %
                            KeyEvent/ANY
                            (fx/event-handler key-handler))
                           (-> % .focusedProperty (.addListener stage-focus-l))
                           %)})))

(defmethod build-view ::top-level
  [{:keys [children]}]
  {:fx/type :fx/top-level
   :children (mapv build-view children)})

(defmethod build-view ::cell
  [{:keys [id cell]}]
  (let [component (build-view cell)]
    (fx/set-fields! component {:id          id
                               :style-class ["focusable"]}) ;;TODO add style-class instead of replacing the whole list
    (register-component! id component)
    component))

;;;;;;;;;;;;;;;;

(defn- layout-children [{:keys [children root] :as node}]
  (or children (when root [root])))

(defn layout-zipper [root]
  (zip/zipper #(some? (layout-children %))
              layout-children
              (fn [node children]
                (if (:children node)
                  (assoc node :children (vec children))
                  (assoc node :root (first children)))) root))

(defn find-focused [tree]
  (some->> (tree-seq layout-children layout-children tree)
           (filter :focused?)
           first))

(defn find-by-id [tree id]
  (some->> (tree-seq layout-children layout-children tree)
           (filter #(= id (:id %)))
           first))

(defn node-in-direction [node-id direction tree]
  (when node-id
    (let [mapping       (c/value state/view-to-component)
          bounds-for-id #(-> mapping (get %) fx/bounds-in-screen)
          {:keys [min-x min-y
                  max-x max-y
                  width height]
           :as bbox}    (bounds-for-id node-id)
          hor?          (contains? #{:left :right} direction)
          min           (if hor? min-x min-y)
          #_             (do (prn 'BBOX bbox)
                            (prn 'MIN min))
          max           (if hor? max-x max-y)
          pos           (if hor?
                          (+ min-y (/ height 2))
                          (+ min-x (/ width 2)))
          in-direction? (fn [{:keys [id]}]
                          (let [{:keys [min-x min-y max-x max-y] :as bbox}
                                (bounds-for-id id)]
                            (condp = direction
                              :up    (<= max-y min)
                              :down  (<= max min-y)
                              :left  (<= max-x min)
                              :right (<= max min-x))))
          covers-pos?   (fn [{:keys [id]}]
                          (let [{:keys [min-x min-y max-x max-y]}
                                (bounds-for-id id)]
                            (if hor?
                              (<= min-y pos max-y)
                              (<= min-x pos max-x))))
          candidates    (some->>
                         (tree-seq layout-children layout-children tree)
                         (filter :focusable?)
                         (remove #(= node-id (:id %)))
                         (filter (partial in-direction?)))]
      (or (some->> (filter covers-pos? candidates) first :id)
          (-> candidates first :id)))))

(require '[clojure.pprint :as pp])
(defn update-layout! [old-tree new-tree]
  (let [diff (util/tree-diff
              (build-view old-tree)
              (build-view new-tree))]
    (fx/update-tree :fx/top-level diff)))
