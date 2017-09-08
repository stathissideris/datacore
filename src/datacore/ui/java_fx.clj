(ns datacore.ui.java-fx
  (:refer-clojure :exclude [parents methods tree-seq])
  (:require [clojure.string :as str]
            [clojure.spec :as s]
            [datacore.util :as util]
            [datacore.cells :as c]
            [clojure.walk :as walk]
            [hawk.core :as hawk]
            [me.raynes.fs :as fs])
  (:import [javafx.collections ObservableList ListChangeListener]
           [javafx.scene.control SplitPane TableView ListView]
           [javafx.embed.swing JFXPanel]
           [javafx.application Platform]
           [javafx.stage StageStyle]
           [javafx.event EventHandler Event]
           [javafx.scene.paint Color]
           [javafx.beans.value ChangeListener]
           [javafx.scene Node]
           [javafx.scene.input Clipboard ClipboardContent]
           [javafx.concurrent Worker]
           [com.sun.javafx.stage StageHelper]
           [javafx.util Callback]
           [javafx.scene.text Font FontWeight FontPosture TextAlignment TextFlow]
           [javafx.scene.web WebView]
           [java.net URI]
           [org.w3c.dom.events EventListener]
           [javax.swing ImageIcon]))

(defonce force-toolkit-init (javafx.embed.swing.JFXPanel.))

;;;; utils ;;;;;

(defn init []
  (Platform/setImplicitExit false))

(defn on-fx-thread? []
  (Platform/isFxApplicationThread))

(defn run-later! [fun]
  (let [p (promise)]
    (if (on-fx-thread?)
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

(defn one-off-change-listener
  ([fun]
   (reify ChangeListener
     (changed [this observable old new]
       (fun observable old new)
       (when new ;;remove only of the value changes to something non-nil and not false
         (.removeListener observable this))))))

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

(defn bounds-in-parent [component]
  (parse-bbox (.getBoundsInParent component)))

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
            (.setAll (getter object) (remove nil? value))
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

(declare set-field-in!)
(defn set-field! [object field value]
  (when object
    (cond
      (and (= object :fx/top-level) (= field :children))
      (StageHelper/getStages)

      (int? field) ;;ObservableList
      (.set object field value)

      (vector? field)
      (set-field-in! object field value)

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

(defn- reload-stylesheet! [component path]
  (doto component
    (-> .getStylesheets (.remove path))
    (-> .getStylesheets (.add path))))

(defn- watch-sheet! [component path]
  (let [wp (some-> path URI. fs/file .getPath)]
    {:path         path
     :watcher-path wp
     :file-watcher
     (hawk/watch! [{:paths   [wp]
                    :handler (fn [_ _]
                               (run-later!
                                #(reload-stylesheet! component path)))}])}))

(defmethod fset [Object :fx/stylesheets]
  [o _ paths]
  (let [paths (map util/resource->external-form paths)]
    (doto o
      (-> .getStylesheets .clear)
      (-> .getStylesheets (.addAll paths))
      (util/alter-meta! assoc :stylesheets (mapv #(watch-sheet! o %) paths)))))

(defmethod fset [WebView :fx/stylesheet]
  [o _ path]
  (let [path (util/resource->external-form path)
        wp   (some-> path URI. fs/file .getPath)]
    (-> o .getEngine (.setUserStyleSheetLocation path))
    (util/alter-meta!
     o assoc :stylesheet
     {:path         path
      :watcher-path wp
      :watcher      (hawk/watch! [{:paths   [wp]
                                   :handler (fn [_ _]
                                              (run-later!
                                               #(doto o
                                                  (-> .getEngine (.setUserStyleSheetLocation nil))
                                                  (-> .getEngine (.setUserStyleSheetLocation path)))))}])})))

(defmulti fget (fn [o field] [(class o) field]))

(defmethod fget [ListView :fx/visible-range]
  [o _]
  (let [virtual-flow (some-> o .getSkin .getChildren (.get 0))]
    [(some-> virtual-flow .getFirstVisibleCell .getIndex)
     (some-> virtual-flow .getLastVisibleCell .getIndex)]))

(defmethod fget [TableView :fx/visible-range]
  [o _]
  (let [virtual-flow (some-> o .getSkin .getChildren (.get 1))]
    [(some-> virtual-flow .getFirstVisibleCell .getIndex)
     (some-> virtual-flow .getLastVisibleCell .getIndex)]))

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

(extend-type javafx.scene.control.ScrollPane
  Parent
  (children [this]
    [(.getContent this)])
  (children? [this]
    true)
  (child [this index]
    (.getContent this)))

(extend-type javafx.scene.Group
  Parent
  (children [this]
    (.getChildren this))
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
     (when-let [m (not-empty (util/meta root))]
       {:meta m})
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

(defn lookup [component selector]
  (into [] (-> component (.lookupAll selector) .toArray)))

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
  {"black"       FontWeight/BLACK
   "bold"        FontWeight/BOLD
   "extra-bold"  FontWeight/EXTRA_BOLD
   "extra-light" FontWeight/EXTRA_LIGHT
   "light"       FontWeight/LIGHT
   "medium"      FontWeight/MEDIUM
   "normal"      FontWeight/NORMAL
   "semi-bold"   FontWeight/SEMI_BOLD
   "thin"        FontWeight/THIN})

(def font-posture-map
  {"italic"  FontPosture/ITALIC
   "regular" FontPosture/REGULAR})

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

(defn span [{:keys [underline strike align fill] :as attr} content]
  (let [font-attr (not-empty (select-keys attr [:family :weight :posture :size]))
        f         (when font-attr (font font-attr))
        text
        (doto (javafx.scene.text.Text. content)
          (.setUnderline (or underline false))
          (.setStrikethrough (or strike false))
          (.setTextAlignment (text-alignment-map (or align :left))))]
    (when f (.setFont text f))
    (when fill
      (.setFill text fill))
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
     (TextFlow. (into-array Node (map text (remove nil? nodes)))))))

(defmethod fset [TextFlow :fx/children]
  [tf _ nodes]
  (-> tf .getChildren (.setAll (mapv text (remove nil? nodes)))))

;;;;;;;;;;;;;;;;;;;; color ;;;;;;;;;;;;;;;;;;;;

(defn color
  ([web]
   (Color/web web))
  ([r g b]
   (Color/color r g b))
  ([r g b a]
   (Color/color r g b a)))

(defn darker [color] (.darker color))
(defn brighter [color] (.brighter color))
(defn desaturated [color] (.desaturate color))
(defn saturated [color] (.saturate color))
(defn grayscale [color] (.grayscale color))
(defn inverted [color] (.invert color))

;;;;;;;;;;;;;;;;;;;; clipboard ;;;;;;;;;;;;;;;;;;;;

(defn to-clipboard [text]
  (doto (Clipboard/getSystemClipboard)
    (.setContent (doto (ClipboardContent.)
                   (.putString text)))))

;;;;;;;;;;;;;;;;;;;; web ;;;;;;;;;;;;;;;;;;;;

(defn dom-event-listener [fun]
  (reify EventListener
    (^void handleEvent [_ ^org.w3c.dom.events.Event event]
     (fun event))))

;;from: http://blogs.kiyut.com/tonny/2013/07/30/javafx-webview-addhyperlinklistener/
(defmethod fset [WebView :fx/link-listener]
  [this _ listener]
  (when listener
    (-> this .getEngine .getLoadWorker .stateProperty
        (.addListener
         (change-listener
          (fn [_ old new]
            (when (= new javafx.concurrent.Worker$State/SUCCEEDED)
              (let [event-listener (dom-event-listener
                                    (fn [event]
                                      (when-let [href (some-> event .getTarget (.getAttribute "href"))]
                                        (listener event href (keyword (.getType event))))))
                    nodes (-> this .getEngine .getDocument (.getElementsByTagName "a"))]
                (doseq [idx (range (.getLength nodes))]
                  (.addEventListener (.item nodes idx) "click" event-listener false)
                  (.addEventListener (.item nodes idx) "mouseover" event-listener false))))))))))

;;see com.sun.deploy.uitoolkit.impl.fx.HostServicesFactory/showDocument
(defn open-in-browser [url]
  (let [os (System/getProperty "os.name")]
    (cond
      (str/starts-with? os "Mac OS")
      (eval `(com.apple.eio.FileManager/openURL ~url))
      (str/starts-with? os "Windows")
      (-> (Runtime/getRuntime) (.exec (str "rundll32 url.dll,FileProtocolHandler " url)))
      :else
      (let [browsers ["google-chrome" "firefox" "opera" "konqueror" "mozilla"]]
        ;;TODO linux: use Runtime.getRuntime().exec() to find which browser is installed
        ;;and use the same method to invoke the browser with the passed URL
        ))))

;;TODO this is interesting as well:
;; (-> (com.apple.eawt.Application/getApplication)
;;     (.setDockIconBadge "2"))

(defn set-app-icon [resource-path]
  (let [os (System/getProperty "os.name")]
    (cond
      (str/starts-with? os "Mac OS")
      (eval
       `(let [icon-url# (.getResource Class ~resource-path)
              image#    (.getImage (ImageIcon. icon-url#))]
          (-> (com.apple.eawt.Application/getApplication)
              (.setDockIconImage image#))))
      (str/starts-with? os "Windows")
      ;;TODO
      :else
      ;;TODO linux
      )))

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
