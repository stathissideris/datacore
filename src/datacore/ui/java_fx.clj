(ns datacore.ui.java-fx
  (:require [datacore.util :as util]
            [clojure.string :as str]
            [datacore.cells :as c])
  (:import [javafx.collections ObservableList]
           [javafx.application Platform]
           [javafx.stage StageStyle]
           [javafx.scene.paint Color]))

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

(defn- getter-method [clazz field-kw]
  (let [method-name (->> (util/kebab->camel field-kw)
                         str/capitalize
                         (str "get"))]
    (first (filter #(= (.getName %) method-name) (-> clazz .getMethods seq)))))

(defn- getter [clazz field-kw]
  (when-let [getter (getter-method clazz field-kw)]
    (fn [object] (.invoke getter object (object-array [])))))

(defn- setter-method [clazz field-kw]
  (let [method-name (->> (util/kebab->camel field-kw)
                         str/capitalize
                         (str "set"))]
    (first (filter #(= (.getName %) method-name) (-> clazz .getMethods seq)))))

(defn- setter [clazz field-kw]
  (if-let [setter (setter-method clazz field-kw)]
    (fn [object value]
      (.invoke setter object (object-array [value]))
      object)
    (when-let [getter-method (getter-method clazz field-kw)]
      (when (= ObservableList (.getReturnType getter-method))
        (let [getter (getter clazz field-kw)]
          (fn [object value]
            (.setAll (getter object) value)
            object))))))

(defn get-field [object field-kw]
  ((getter (class object) field-kw) object))

(defn set-field! [object field-kw value]
  (try
    (clojure.lang.Reflector/invokeInstanceMethod
     object
     (->> field-kw
          util/kebab->camel
          util/capitalize-first
          (str "set"))
     (object-array [value]))
    (catch Exception _
      (if (c/cell-id? value)
        (do
          (run-later! #(set-field! object field-kw (c/value value)))
          (when-not (c/label value)
            (c/set-label! value (keyword (str (.getName (class object)) "-" (name field-kw)))))
          (c/add-watch!
           value
           [object field-kw]
           (fn [_ _ v]
             (run-later! #(set-field! object field-kw v)))))
        (let [s! (setter (class object) field-kw)]
          (if-not s!
            (throw (ex-info "setter not found"
                            {:object object
                             :class  (class object)
                             :field  field-kw
                             :value  value}))
            (try
              (s! object value)
              (catch Exception e
                (throw (ex-info "error while calling setter"
                                {:object object
                                 :class  (class object)
                                 :field  field-kw
                                 :value  value}))))))))))

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
   (let [clazz (if (keyword? class-kw)
                 (resolve-class class-kw)
                 class-kw)]
     (if (empty? args)
       (.newInstance clazz)
       (clojure.lang.Reflector/invokeConstructor clazz (to-array args))))))

;;;;; make ;;;;;

(defn make-args [spec]
  (vec (second (first (filter #(= (first %) :fx/args) spec)))))

(defn make-other [spec]
  (remove #(= (first %) :fx/args) spec))

(defn make
  ([class-or-instance]
   (make class-or-instance {}))
  ([class-or-instance spec]
   (let [o (if (or (keyword? class-or-instance)
                   (class? class-or-instance))
             (new-instance class-or-instance (make-args spec))
             class-or-instance)]
     (doseq [[field value :as entry] (make-other spec)]
       (when entry
         (if (= field :fx/setup)
           (value o)
           (set-field! o field value))))
     o)))

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

(defn show [c] (.show c) c)

(comment
  (make
   :scene.control/button
   [[:fx/args ["foo"]]
    [:text "bar"]
    [:fx/setup #(.setText % "baz")]]))

(comment
  (def s (-> (new-instance :scene.control/split-pane)
             (set-field! :items [(new-instance :scene.control/button ["1"])
                                 (new-instance :scene.control/button ["2"])
                                 (new-instance :scene.control/button ["3"])])))
  (get-field s :items))
