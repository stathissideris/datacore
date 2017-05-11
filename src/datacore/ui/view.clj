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
            [datacore.ui.timer :as timer]
            [datacore.state :as state]
            [clojure.walk :as walk])
  (:import [javafx.scene.input KeyEvent MouseEvent]
           [javafx.scene.paint Color]
           [javafx.stage StageStyle]))

(defn focus! [component]
  (if-let [c (some-> component
                     (fx/find-by-style-class "main-component")
                     (first))]
    (fx/run-later! #(.requestFocus c))
    (fx/run-later! #(.requestFocus component)))
  component)

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

(defmethod fx/fset :dc/indicate-focus?
  [component _ focused?]
  (fx/set-field! component :style (if focused? focused-style unfocused-style)))

(declare focus!)
(defn- build-nothing []
  {:fx/type            :scene.layout/border-pane
   :style-class        ["focus-indicator"]
   :center             {:fx/type           :scene.control/label
                        :style-class       ["label" "main-component"]
                        :text              "Nothing to show"
                        :focus-traversable true}
   :on-mouse-clicked   (fx/event-handler
                        (fn [e]
                          (focus! (.getTarget e))))})

(defmethod build-view ::nothing
  [tree]
  (build-nothing))

(defmethod build-view nil
  [tree]
  (build-nothing))

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

;;TODO add this to scene: [:fx/setup #(style/add-stylesheet % "css/default.css")]

(defn focus-indicator-parent [component]
  (some->> component fx/parents (filter #(fx/has-style-class? % "focus-indicator")) first))

(defmethod build-view ::window
  [{:keys [id title dimensions root window-style]}]
  (let [[width height] dimensions
        key-handler    (keys/key-handler #'default-keys/root-keymap)
        scene-args     (concat
                        [(build-view ::nothing)]
                        (when dimensions [width height]))
        scene-focus-l  (fx/change-listener
                        (fn [_ old new]
                          (println "COMPONENT LOST FOCUS:" (fx/tree new))
                          (println "COMPONENT FOCUSED:" (fx/tree new))
                          (when new
                            (fx/set-field! (focus-indicator-parent old) :dc/indicate-focus? false)
                            (fx/set-field! (focus-indicator-parent new) :dc/indicate-focus? true))))
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
                          :fx/setup #(-> % .focusOwnerProperty (.addListener scene-focus-l))}
                         (when (= window-style :transparent)
                           {:fill Color/TRANSPARENT}))
      :fx/event-filter  [KeyEvent/ANY key-handler]
      :fx/setup         #(-> % .focusedProperty (.addListener stage-focus-l))})))

(defmethod build-view ::cell
  [{:keys [id cell focused?]}]
  (let [view (build-view cell)]
    (-> view
        (fx/set-fields!
         {:id                 id
          :style-class        ["focus-indicator"]
          :dc/indicate-focus? focused?
          :fx/event-filter    [MouseEvent/MOUSE_CLICKED #(focus! id)]})
        fx/unmanaged)))

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

(defn focusable-in-direction [component direction]
  (when component
    (let [{:keys [min-x min-y
                  max-x max-y
                  width height]
           :as bbox}    (fx/bounds-in-screen component)
          hor?          (contains? #{:left :right} direction)
          min           (if hor? min-x min-y)
          #_             (do (prn 'BBOX bbox)
                            (prn 'MIN min))
          max           (if hor? max-x max-y)
          pos           (if hor?
                          (+ min-y (/ height 2))
                          (+ min-x (/ width 2)))
          in-direction? (fn [component]
                          (let [{:keys [min-x min-y max-x max-y] :as bbox}
                                (fx/bounds-in-screen component)]
                            (condp = direction
                              :up    (<= max-y min)
                              :down  (<= max min-y)
                              :left  (<= max-x min)
                              :right (<= max min-x))))
          covers-pos?   (fn [component]
                          (let [{:keys [min-x min-y max-x max-y]}
                                (geom/extend-bbox (fx/bounds-in-screen component) 20)] ;;extend to avoid getting stuck on separators
                            (if hor?
                              (<= min-y pos max-y)
                              (<= min-x pos max-x))))
          candidates    (some->>
                         (fx/find-by-style-class fx/top-level "focus-indicator")
                         (remove #(= % component))
                         (filter (partial in-direction?))
                         (sort-by #(geom/bbox-distance bbox (fx/bounds-in-screen %))))]
      (or (some->> (filter covers-pos? candidates) first)
          (first candidates)))))

(comment
 (defn- handle-focus! [old-tree new-tree]
   (let [c (some-> (find-focused old-tree)
                   :id
                   (fx/find-by-id))]
     (when c (fx/set-field! c :indicate-focus false)))

   (let [focus-id (some-> (find-focused new-tree) :id)
         c        (fx/find-by-id focus-id)]
     (when (and c (not= c fx/top-level))
       (fx/set-field! c :indicate-focus false)
       ;;TODO nasty hack: The delay is necessary when doing
       ;;window/split. The component that should get the focus is
       ;;momentarily without a Scene, so its request for focus is
       ;;rejected and the focus is instead given to a different
       ;;component. Not sure how to fix properly. See implementation of
       ;;javafx.scene.Node/requestFocus
       (timer/delayed 20 #(focus-cell-view-main! c))))))
