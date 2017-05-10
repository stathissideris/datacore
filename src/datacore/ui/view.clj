(ns datacore.ui.view
  (:require [clojure.core.match :as m :refer [match]]
            [clojure.zip :as zip]
            [clojure.spec :as s]
            [datacore.ui.java-fx :as fx]
            [datacore.util :as util]
            [datacore.util.geometry :as geom]
            [datacore.cells :as c]
            [datacore.ui.keys :as keys]
            [datacore.ui.keys.defaults :as default-keys]
            [datacore.ui.message :as message]
            [datacore.state :as state]
            [clojure.walk :as walk])
  (:import [javafx.scene.input KeyEvent MouseEvent]
           [javafx.scene.paint Color]
           [javafx.stage StageStyle]))

(def focused-style (str "-fx-border-width: 4 4 4 4;"
                        "-fx-border-color: #155477;"))
(def unfocused-style "-fx-border-width: 0 0 0 0;")

(def components-cache (atom {}))
(defn- memo-component [id fun]
  (or (-> components-cache deref (get id))
      (let [res (fun)]
        (swap! components-cache assoc id res)
        res)))

(defmulti build-view (fn [x] (or (:type x)
                                 (when (c/cell-id? x) (:type (c/value x))))))

(defn set-focus-border! [component focused?]
  (fx/set-field! component :style (if focused? focused-style unfocused-style)))

(defn label [text]
  {:fx/type :scene.control/label
   :text    text})

(declare focus)
(defn- build-nothing [{:keys [id focused?]}]
  {:fx/type          :scene.layout/border-pane
   :id               id
   :style-class      ["focusable"]
   :style            (if focused? focused-style unfocused-style)
   :center           (label "Nothing to show")
   :on-mouse-clicked (fx/event-handler
                      (fn [_]
                        (focus id)))})

(defmethod build-view ::nothing
  [tree]
  (build-nothing tree))

(defmethod build-view nil
  [tree]
  (build-nothing tree))

(defmethod build-view ::split-pane
  [{:keys [orientation children]}]
  {:fx/type     :scene.control/split-pane
   :items       (map (comp build-view) children)
   :orientation (if (= orientation :horizontal)
                  javafx.geometry.Orientation/HORIZONTAL
                  javafx.geometry.Orientation/VERTICAL)})

(defn- message-line []
  (memo-component
   "message-line"
   #(->> {:fx/type   :scene.control/label
          :text      (c/formula :msg message/current-message)
          :style     "-fx-padding: 0.6em 0.6em 0.6em 0.6em;"
          :text-fill (c/formula (comp {:message Color/BLACK
                                       :error   (Color/web "0xF57000")}
                                      :type) message/current-message)}
         fx/make-tree
         fx/unmanaged)))

(defn build-window-contents [tree message]
  {:fx/type :scene.layout/border-pane
   :center  (if-not tree
              (build-view {:type ::nothing})
              (build-view tree))
   :bottom  (message-line)})

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
                            (println "will attempt to focus " component-id)
                            (focus component-id))
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
  [{:keys [id cell focused?]}]
  (-> (memo-component
       id
       #(-> cell
            build-view
            (fx/set-fields! {:id               id
                             ;;TODO add style-class instead of replacing the whole list
                             :style-class      ["focusable"]
                             :style            (if focused? focused-style unfocused-style)})
            (doto
              (.addEventFilter
               MouseEvent/MOUSE_CLICKED
               (fx/event-handler (fn [_] (focus id)))))
            fx/unmanaged))
      (update :fx/component set-focus-border! focused?)))

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

(defn focus [focus-id]
  (when focus-id
    (state/swap-layout!
     (fn [tree]
       (walk/postwalk
        (fn [{:keys [id] :as x}]
          (if (map? x)
            (if (= id focus-id)
              (assoc x :focused? true)
              (dissoc x :focused?))
            x))
        tree)))))

(defn node-in-direction [node-id direction tree]
  (when node-id
    (let [bounds-for-id #(-> % fx/find-by-id fx/bounds-in-screen)
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
                                (geom/extend-bbox (bounds-for-id id) 20)] ;;extend to avoid getting stuck on separators
                            (if hor?
                              (<= min-y pos max-y)
                              (<= min-x pos max-x))))
          candidates    (some->>
                         (tree-seq layout-children layout-children tree)
                         (filter :focusable?)
                         (remove #(= node-id (:id %)))
                         (filter (partial in-direction?))
                         (sort-by #(geom/bbox-distance bbox (bounds-for-id (:id %)))))]
      (or (some->> (filter covers-pos? candidates) first :id)
          (-> candidates first :id)))))

(require '[clojure.pprint :as pp])
(defn update-layout! [old-tree new-tree]
  (let [diff (util/tree-diff
              (build-view old-tree)
              (build-view new-tree))]
    (fx/update-tree :fx/top-level diff)))
