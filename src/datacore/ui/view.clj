(ns datacore.ui.view
  (:require [clojure.core.match :as m :refer [match]]
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

(defn- get-or-build-view [{:keys [id] :as component-map}]
  (prn "get-or-build-view for ID" id)
  (prn "existing component" (-> state/view-to-component c/value (get id)))
  (or (and id (-> state/view-to-component c/value (get id)))
      (build-view component-map)))

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
  (let [components (map get-or-build-view children)]
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

(defn set-focus-border! [component focused?]
  (when component
    (fx/set-field!
     component :style
     (if focused?
       (str "-fx-border-width: 4 4 4 4;"
            "-fx-border-color: #155477;")
       (str "-fx-border-width: 0 0 0 0;")))
    component))

(defn build-window-contents [{:keys [focused?] :as tree} message]
  (let [component (if-not tree
                    (build-view ::nothing)
                    (get-or-build-view tree))]
   (fx/make
    :scene.layout/border-pane
    {:center (set-focus-border! component focused?)
     :bottom (message-line message)})))

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
                           (get-or-build-view (or root ::nothing))
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
            (when-let [component-id (when new
                                      (some->> (cons new (fx/parents new))
                                               (filter #(fx/has-style-class? % "focusable"))
                                               first
                                               .getId))]
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

(defn- layout-children [{:keys [children root] :as node}]
  (or children (when root [root])))

(defn find-focused [tree]
  (some->> (tree-seq layout-children layout-children tree)
           (filter :focused?)
           first))

(defn component-in-direction [component-id direction tree]
  (when component-id
    (let [mapping                (c/value state/view-to-component)
          bounds-for-id          #(some-> mapping (get %) fx/bounds-in-screen)
          {:keys [min-x min-y
                  max-x max-y
                  width height]} (bounds-for-id component-id)
          hor?                   (#{:left :right} direction)
          min                    (if hor? min-x min-y)
          max                    (if hor? max-x max-y)
          pos                    (if hor?
                                   (+ min-y (/ height 2))
                                   (+ min-x (/ width 2)))
          in-direction?          (fn [{:keys [id]}]
                                   (let [{:keys [min-x min-y max-x max-y]}
                                         (bounds-for-id id)]
                                     (condp = direction
                                       :up    (<= max-y min)
                                       :down  (<= max min-y)
                                       :left  (<= max-x min)
                                       :right (<= max min-x))))
          covers-pos?            (fn [{:keys [id]}]
                                   (let [{:keys [min-x min-y max-x max-y]}
                                         (bounds-for-id id)]
                                     (if hor?
                                       (<= min-y pos max-y)
                                       (<= min-x pos max-x))))
          candidates             (some->>
                                  (tree-seq layout-children layout-children tree)
                                  (filter :focusable?)
                                  (remove #(= component-id (:id %)))
                                  (filter (partial in-direction?)))]
      (or (some->> (filter covers-pos? candidates) first :id)
          (-> candidates first :id)))))

(defn- scene-for-path [path]
  (let [window-index (second path)
        id           (some-> state/layout-tree c/value (get-in [:children window-index :id]))]
    (some-> (c/value state/view-to-component)
            (get id)
            .getScene)))

(defn- diff-type [{:keys [type path] :as diff}]
  (cond (= :root (-> path butlast last))
        :set-root

        (and (= type :insert)
             (= :children (first path)))
        :add-window

        (and (= type :delete)
             (= :children (first path)))
        :delete-window

        (and (= :children (first path))
             (= :title (last path))
             (= 3 (count path)))
        :set-window-title

        (= :focused? (last path))
        :set-focus

        (and (#{:assoc :edit} type)
             (int? (-> path butlast last)))
        :set-fields

        :else
        :default))

(require '[clojure.pprint :as pp])
(defn update-layout! [old-tree new-tree]
  (let [diffs (->> (util/tree-diff old-tree new-tree)
                   (map #(assoc % :diff-type (diff-type %)))
                   (partition-by :diff-type))]
    (pp/pprint diffs)
    (doseq [diff-group diffs]
      (let [{:keys [diff-type path old value] :as diff} (first diff-group)]
        (condp = diff-type

          :add-window
          (let [component @(fx/run-later! #(build-view value))]
            (register-component! (:id value) component)
            (fx/run-later! #(fx/show component)))

          :delete-window
          (let [component (get (c/value state/view-to-component) (:id value))]
            (when component
              (unregister-component! (:id value))
              (fx/run-later! #(.close component))))

          :set-root
          (let [component-map (get-in new-tree (butlast path))
                scene         (scene-for-path path)]
            (pp/pprint component-map)
            (fx/run-later!
             #(.setRoot scene (build-window-contents component-map message/current-message))))

          :set-fields
          (let [component-map (get-in new-tree (butlast path))]
            (pp/pprint component-map)
            (fx/run-later!
             (fn []
               (fx/set-field-in! (scene-for-path path)
                                 (mapcat #(cond (= % :root) [:root :center] ;;translate datacore path to javafx path
                                                (= % :children) [:items]
                                                :else [%])
                                         (nnext (butlast path)))
                                 (get-or-build-view component-map)))))

          :set-focus
          (doseq [{:keys [type path]} diff-group]
            (let [id        (get-in (c/value state/layout-tree) (conj (vec (butlast path)) :id))
                  component (get (c/value state/view-to-component) id)]
              (prn path id (= type :assoc) component)
              (set-focus-border! component (= type :assoc))))

          :set-window-title
          (fx/run-later! #(-> (scene-for-path path)
                              .getWindow
                              (.setTitle value)))

          :skip)))))
