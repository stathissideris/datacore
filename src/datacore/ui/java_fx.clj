(ns datacore.ui.java-fx
  (:refer-clojure :exclude [parents methods tree-seq])
  (:require [clojure.string :as str]
            [clojure.spec :as s]
            [datacore.util :as util]
            [datacore.cells :as c]
            [clojure.walk :as walk])
  (:import [javafx.collections ObservableList ListChangeListener]
           [javafx.scene.control.SplitPane]
           [javafx.embed.swing JFXPanel]
           [javafx.application Platform]
           [javafx.stage StageStyle]
           [javafx.event EventHandler Event]
           [javafx.scene.paint Color]
           [javafx.beans.value ChangeListener]
           [javafx.scene Node]
           [com.sun.javafx.stage StageHelper]
           [javafx.util Callback]
           [javafx.scene.text Font FontWeight FontPosture TextAlignment TextFlow]))

(defonce force-toolkit-init (javafx.embed.swing.JFXPanel.))

;;;; utils ;;;;;

(defn init []
  (Platform/setImplicitExit false))

(defn run-later! [fun]
  (let [p (promise)]
    (if (Platform/isFxApplicationThread)
      (deliver p (fun))
      (Platform/runLater
       (fn []
         (try
           (deliver p (fun))
           (catch Exception e
             (.printStackTrace e)
             (throw e))))))
    p))

(defn event-handler [fun]
  (reify EventHandler
    (^void handle [_ ^Event event]
     (fun event))))

(defn change-listener
  ([fun]
   (reify ChangeListener
     (changed [_ observable old new]
       (fun observable old new))))
  ([source fun]
   (reify ChangeListener
     (changed [_ observable old new]
       (fun source observable old new)))))

(defn list-change-listener [fun]
  (reify ListChangeListener
    (onChanged [this change]
      (fun (seq (.getList change))))))

(defn callback [fun]
  (reify Callback
    (call [this p]
      (fun p))))

(defn parse-bbox [bbox]
  {:min-x  (.getMinX   bbox)
   :max-x  (.getMaxX   bbox)
   :min-z  (.getMinZ   bbox)
   :width  (.getWidth  bbox)
   :max-z  (.getMaxZ   bbox)
   :depth  (.getDepth  bbox)
   :max-y  (.getMaxY   bbox)
   :min-y  (.getMinY   bbox)
   :height (.getHeight bbox)})

(defn bounds-in-screen [component]
  (parse-bbox (.localToScreen component (.getBoundsInLocal component))))

(defn bounds-in-scene [component]
  (parse-bbox (.localToScene component (.getBoundsInLocal component))))

;;;;; reflection ;;;;;

(defn- superclasses [clazz]
  (when-let [super (.getSuperclass clazz)]
    (cons super (lazy-seq (superclasses super)))))

(defn- methods [^Class class]
  (.getMethods class))

(defn- getter-method [clazz field-kw]
  (let [method-name (->> (util/kebab->camel field-kw)
                         util/capitalize-first
                         (str "get"))]
    (first (filter #(= (.getName %) method-name) (mapcat methods (cons clazz (superclasses clazz)))))))

(defn- getter [clazz field-kw]
  (when-let [getter (getter-method clazz field-kw)]
    (fn [object] (.invoke getter object (object-array [])))))

(defn- setter-method [clazz field-kw]
  (let [method-name (->> (util/kebab->camel field-kw)
                         util/capitalize-first
                         (str "set"))]
    (first (filter #(= (.getName %) method-name) (mapcat methods (cons clazz (superclasses clazz)))))))

(defn- setter [clazz field-kw]
  (if-let [setter (setter-method clazz field-kw)]
    (fn [object value]
      (.invoke setter object (object-array [value]))
      object)
    (when-let [getter-method (getter-method clazz field-kw)]
      (when (= ObservableList (.getReturnType getter-method)) ;;support for setting observable list fields
        (let [getter (getter clazz field-kw)]
          (fn [object value]
            (.setAll (getter object) value)
            object))))))

(defn get-field [object field-kw]
  ((getter (class object) field-kw) object))

(defmulti fset (fn [o field _] [(class o) field]))

(defmethod fset [Object :fx/setup]
  [o _ value]
  (value o)
  o)

;;(def set-focused! (declared-method javafx.scene.Node "setFocused" [Boolean/TYPE]))
(defmethod fset [Object :fx/focused?]
  [o _ focus?]
  (when focus?
    (run-later! #(.requestFocus o))))

(defmethod fset [Object :fx/event-filter]
  [o _ [filter fun]]
  (.addEventFilter o filter (event-handler fun)))

(defn get-property [object field]
  (clojure.lang.Reflector/invokeInstanceMethod
   object
   (str (util/kebab->camel field) "Property")
   (object-array [])))

(defmethod fset [Object :fx/prop-listener]
  [o _ [prop fun]]
  (.addListener (get-property o prop) (change-listener o fun)))

(defn set-field! [object field value]
  (when object
    (cond
      (and (= object :fx/top-level) (= field :children))
      (StageHelper/getStages)

      (int? field) ;;ObservableList
      (.set object field value)

      (namespace field)
      (fset object field value)

      :else
      (try
        (clojure.lang.Reflector/invokeInstanceMethod
         object
         (->> field
              util/kebab->camel
              util/capitalize-first
              (str "set"))
         (object-array [value]))
        object
        (catch Exception _
          (if (c/cell-id? value)
            (do
              (run-later! #(set-field! object field (c/value value)))
              (when-not (c/label value)
                (c/set-label! value (keyword (str (.getName (class object)) "-" (name field)))))
              (c/add-watch!
               value
               [object field]
               (fn [_ _ v]
                 (run-later! #(set-field! object field v))))
              object)
            (let [s! (setter (class object) field)]
              (if-not s!
                (throw (ex-info "setter not found"
                                {:object object
                                 :class  (class object)
                                 :field  field
                                 :value  value}))
                (try
                  (s! object value)
                  object
                  (catch Exception e
                    (throw (ex-info "error while calling setter"
                                    {:object object
                                     :class  (class object)
                                     :field  field
                                     :value  value})))))))))))
  object)

(defmulti fget (fn [o field] [(class o) field]))

(defmethod fget [Object :fx/visible-range]
  [o _]
  (let [virtual-flow (some-> o .getSkin .getChildren (.get 0))]
    [(-> virtual-flow .getFirstVisibleCell .getIndex)
     (-> virtual-flow .getLastVisibleCell .getIndex)]))

(defn get-field [object field]
  (cond (and (= object :fx/top-level) (= field :children))
        (StageHelper/getStages)

        (int? field) ;;ObservableList
        (.get object field)

        (namespace field)
        (fget object field)

        :else
        (clojure.lang.Reflector/invokeInstanceMethod
         object
         (->> field
              util/kebab->camel
              util/capitalize-first
              (str "get"))
         (object-array []))))

(s/def ::path (s/coll-of (s/or :field-name keyword? :index nat-int?)))
(defn get-field-in [root path]
  (reduce (fn [o field]
            (try
              (get-field o field)
              (catch Exception e
                (throw (ex-info "get-field-in failed"
                                {:path           path
                                 :root           root
                                 :current-object o
                                 :current-field  field}
                                e))))) root path))
(s/fdef get-field-in
  :args (s/cat :root some? :path ::path))

(defn set-field-in! [root path value]
  (let [field       (last path)
        parent-path (butlast path)]
    (try
      (set-field! (get-field-in root parent-path) field value)
      (catch Exception e
        (throw (ex-info "set-field-in! failed"
                        {:path  path
                         :value value
                         :root  root}
                        e))))))
(s/fdef set-field-in!
  :args (s/cat :root some? :path ::path :value any?))

(defn set-fields! [object pairs]
  (doseq [[field value] pairs]
    (set-field! object field value))
  object)

(defn insert-in! [root path value]
  (if (and (= root :fx/top-level) (= 2 (count path)))
    (do
      (.add (StageHelper/getStages) (last path) value)
      (.show value))
    (let [index       (last path)
          parent-path (butlast path)
          coll        (get-field-in root parent-path)]
      (.add coll index value))))

(defn remove-in! [root path]
  (if (and (= root :fx/top-level) (= 2 (count path)))
    (let [stages (StageHelper/getStages)
          value  (.get stages (last path))]
      (.remove stages value)
      (.close value))
    (let [index       (last path)
          parent-path (butlast path)
          coll        (get-field-in root parent-path)
          item        (.get coll index)]
      (.remove coll item)))) ;;removing by index does not work

(defn- resolve-class [class-kw]
  (if (keyword? class-kw)
    (Class/forName (str "javafx."
                        (namespace class-kw)
                        "."
                        (-> class-kw name util/kebab->camel util/capitalize-first)))
    class-kw))

(defn new-instance
  ([class-kw]
   (new-instance class-kw nil))
  ([class-kw args]
   (try
     (let [clazz (if (keyword? class-kw)
                   (resolve-class class-kw)
                   class-kw)]
       (if (empty? args)
         (.newInstance clazz)
         (clojure.lang.Reflector/invokeConstructor clazz (to-array args))))
     (catch Exception e
       (throw (ex-info "Error while creating FX instance"
                       {:class class-kw
                        :args  args}
                       e))))))

;;;;; make ;;;;;

(defn make-args [spec]
  (vec (second (first (filter #(= (first %) :fx/args) spec)))))

(defn make-other [spec]
  (remove #(= (first %) :fx/args) spec))

(defn make
  ([class-or-instance]
   (make class-or-instance {}))
  ([class-or-instance spec]
   (cond (= :fx/top-level class-or-instance)
         :fx/top-level

         (= :fx/unmanaged class-or-instance)
         (:fx/component spec)

         :else
         (let [o (if (or (keyword? class-or-instance)
                         (class? class-or-instance))
                   (new-instance class-or-instance (make-args spec))
                   class-or-instance)]
           (doseq [[field value :as entry] (make-other spec)]
             (when entry (set-field! o field value)))
           o))))

(defn make-tree
  [tree]
  (walk/postwalk
   (fn [item]
     (if (:fx/type item)
       (make (:fx/type item)
             (dissoc item :fx/type))
       item))
   tree))

(defn unmanaged [component]
  {:fx/type      :fx/unmanaged
   :fx/component component})

;;;;; "React" ;;;;;

(defn- type-change? [diff-group]
  (some?
   (first
    (filter #(and (= :edit (:type %))
                  (= :map (:struct %))
                  (= :fx/type (last (:path %)))) diff-group))))

(defn- ignore-diff? [{:keys [type path] :as diff}]
  (or (and (= :edit type) (contains? (set path) :fx/setup))
      (and (= :edit type) (contains? (set path) :fx/args))))

;;(require '[clojure.pprint :refer [pprint]])
(defn update-tree!
  [root diffs]
  ;;(pprint diffs)
  (let [diff-groups (partition-by #(vector (butlast (:path %)) (:struct %)) diffs)]
    (doseq [diff-group diff-groups]
      (cond
        (type-change? diff-group)
        (run-later!
         #(set-field-in! root
                         (-> diff-group first :path butlast)
                         (make-tree (->> diff-group
                                         (filter (comp #{:edit :assoc} :type))
                                         (map (juxt (comp last :path) :value))
                                         (into {})))))
        :else
        (doseq [{:keys [type path value] :as diff} (remove ignore-diff? diff-group)]
          (run-later!
           #(condp = type
              :edit   (set-field-in! root path (make-tree value))
              :assoc  (set-field-in! root path (make-tree value))
              :dissoc (set-field-in! root path nil)
              :insert (insert-in! root path (make-tree value))
              :delete (remove-in! root path))))))))

;;;;; traversal ;;;;;

(defprotocol Parent
  (children? [this])
  (children [this])
  (child [this index]))

(deftype TopLevel []
  Parent
  (children [this]
    (distinct (seq (StageHelper/getStages))))
  (children? [this]
    (< 0 (count (children this))))
  (child [this index]
    (.get (children this) index)))

(def top-level (TopLevel.))

(extend-type javafx.stage.Stage
  Parent
  (children [this] [(.getScene this)])
  (children? [this] true)
  (child [this index] (.getScene this)))

(extend-type javafx.scene.Scene
  Parent
  (children [this] [(.getRoot this)])
  (children? [this] true)
  (child [this index] (.getRoot this)))

(extend-type javafx.scene.Scene
  Parent
  (children [this] [(.getRoot this)])
  (children? [this] true)
  (child [this index] (.getRoot this)))

(extend-type javafx.scene.layout.Pane
  Parent
  (children [this]
    (.getChildren this))
  (children? [this]
    (< 0 (count (children this))))
  (child [this index]
   (.get (children this) index)))

(extend-type javafx.scene.control.SplitPane
  Parent
  (children [this]
    (.getItems this))
  (children? [this]
    (< 0 (count (children this))))
  (child [this index]
    (.get (children this) index)))

(extend-type Object
  Parent
  (children [this] nil)
  (children? [this] false)
  (child [this index] nil))

(defn- safe-id [component]
  (try (.getId component) (catch Exception _ nil)))

(defn tree-seq [root]
  (clojure.core/tree-seq children? children root))

(defn find-by-id
  ([id]
   (find-by-id top-level id))
  ([root id]
   (->> (tree-seq root)
        (filter #(= id (safe-id %)))
        first)))

(defn- safe-style-class [component]
  (try (set (.getStyleClass component))
       (catch Exception _ #{})))

(defn find-by-style-class [root clazz]
  (->> (tree-seq root)
       (filter #(get (safe-style-class %) clazz))))

(defn tree [root]
  (when root
    (merge
     {:component root}
     (when (children? root)
       {:children (mapv tree (children root))}))))

(defn stage-of [component]
  (some-> component .getScene .getWindow))

;;;;; convenience functions ;;;;;

(defn label
  [text & [spec]]
  (make :scene.control/label (merge {:text (str text)} spec)))

(defn window [title root]
  (make :stage/stage
        {:scene (make :scene/scene {:fx/args [root]})}))

(defn transparent-window [root]
  (make :stage/stage
        {:fx/args [StageStyle/TRANSPARENT]
         :scene (make :scene/scene {:fx/args [root]
                                    :fill Color/TRANSPARENT})}))

(defn undecorated-window [root]
  (make :stage/stage
        {:fx/args [StageStyle/UNDECORATED]
         :scene (make :scene/scene {:fx/args [root]})}))

(defn show! [c] (.show c) c)

(defn has-style-class? [node ^String c]
  (and (instance? javafx.scene.Node node)
       (some? (not-empty (filter (partial = c) (seq (.getStyleClass node)))))))

(defn parent [^Node node]
  (when node (.getParent node)))

(defn parents [^Node node]
  (take-while (complement nil?) (rest (iterate parent node))))

(defn focus-owner [stage]
  (some-> stage .getScene .focusOwnerProperty .get))

;;;;;;;;;;;;;;;;;;;; text ;;;;;;;;;;;;;;;;;;;;

(defprotocol Text
  (text [this]))

(extend-type javafx.scene.text.Text
  Text
  (text [this] this))

(extend-type String
  Text
  (text [this] (make :scene.text/text {:fx/args [this]})))

(let [default-font (Font/getDefault)]
  (def font-defaults
    {:family  (.getFamily default-font)
     :weight  FontWeight/NORMAL
     :posture FontPosture/REGULAR
     :size    (.getSize default-font)}))

(def font-weight-map
  {:black       FontWeight/BLACK
   :bold        FontWeight/BOLD
   :extra-bold  FontWeight/EXTRA_BOLD
   :extra-light FontWeight/EXTRA_LIGHT
   :light       FontWeight/LIGHT
   :medium      FontWeight/MEDIUM
   :normal      FontWeight/NORMAL
   :semi-bold   FontWeight/SEMI_BOLD
   :thin        FontWeight/THIN})

(def font-posture-map
  {:italic  FontPosture/ITALIC
   :regular FontPosture/REGULAR})

(defn font [{:keys [family weight posture size] :as options}]
  (let [options (cond-> options
                  weight  (assoc :weight (font-weight-map weight))
                  posture (assoc :posture (font-posture-map posture)))
        {:keys [family weight posture size]}
        (merge font-defaults options)]
    (Font/font family weight posture size)))

(def text-alignment-map
  {:center  TextAlignment/CENTER
   :justify TextAlignment/JUSTIFY
   :left    TextAlignment/LEFT
   :right   TextAlignment/RIGHT})

(defn span [{:keys [underline strike align] :as attr} content]
  (let [font-attr (not-empty (select-keys attr [:family :weight :posture :size]))
        f         (when font-attr (font font-attr))
        text
        (doto (javafx.scene.text.Text. content)
          (.setUnderline (or underline false))
          (.setStrikethrough (or strike false))
          (.setTextAlignment (text-alignment-map (or align :left))))]
    (when f (.setFont text f))
    text))

(extend-type clojure.lang.APersistentVector
  Text
  (text [this]
    (let [tag     (first this)
          attr    (when (map? (second this)) (second this))
          content (if attr (drop 2 this) (rest this))]
      (when (not (every? (some-fn nil? string?) content))
        (throw (ex-info "text hiccup tags cannot be nested" {:tag this})))
      (let [content (apply str (remove nil? content))]
        (condp = tag
          :span (span attr content)
          :b    (span {:weight "bold"} content)
          :i    (span {:posture "italic"} content)
          :u    (span {:underline true} content)
          :del  (span {:strike true} content))))))

(defn text-flow
  ([]
   (TextFlow.))
  ([nodes]
   (if (empty? nodes)
     (TextFlow.)
     (TextFlow. (into-array Node (map text nodes))))))

(comment
  (make
   :scene.control/button
   [[:fx/args ["foo"]]
    [:text "bar"]
    [:fx/setup #(.setText % "baz")]]))

(comment
  (make-tree
   {:fx/type     :scene.control/split-pane
    :orientation javafx.geometry.Orientation/HORIZONTAL
    :items       [{:fx/type :scene.control/label
                   :text    "foo"}
                  {:fx/type :scene.control/label
                   :text    "bar"}
                  {:fx/type :scene.layout/border-pane
                   :center  {:fx/type :scene.control/label
                             :text    "baz"}
                   :bottom  {:fx/type :scene.control/label
                             :text    "zoo"}}]}))

(comment
  (def s (-> (new-instance :scene.control/split-pane)
             (set-field! :items [(new-instance :scene.control/button ["1"])
                                 (new-instance :scene.control/button ["2"])
                                 (new-instance :scene.control/button ["3"])])))
  (get-field s :items))
