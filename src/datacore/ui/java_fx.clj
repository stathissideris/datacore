(ns datacore.ui.java-fx
  (:require [datacore.util :as util]
            [clojure.string :as str])
  (:import [javafx.collections ObservableList]))

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
  ((setter (class object) field-kw) object value))

(defn- constructor [clazz arg-types]
  (let [constructors (-> clazz .getConstructors seq)
        params->con  (dissoc (zipmap (map #(seq (.getParameterTypes %)) constructors)
                                     constructors)
                             nil)]
    (get params->con arg-types)))

(defn- resolve-class [class-kw]
  (Class/forName (str "javafx."
                      (namespace class-kw)
                      "."
                      (-> class-kw name util/kebab->camel util/capitalize-first))))

(defn new-instance
  ([class-kw]
   (new-instance class-kw nil))
  ([class-kw args]
   (let [clazz (if (keyword? class-kw)
                 (resolve-class class-kw)
                 class-kw)]
     (if (empty? args)
       (.newInstance clazz)
       ;;(eval `(new ~(symbol (.getName clazz)) ~@args))
       (if-let [matching-con (constructor clazz (map class args))]
         (.newInstance matching-con (object-array args))
         (throw (ex-info "No matching constructor found for args"
                         {:args      args
                          :arg-types (map class args)})))))))

;;;;; make ;;;;;

(defn make-args [spec]
  (second (first (filter #(= (first %) :fx/args) spec))))

(defn make-other [spec]
  (remove #(= (first %) :fx/args) spec))

(defn make
  ([class-kw]
   (make class-kw {}))
  ([class-kw spec]
   (let [o (new-instance class-kw (make-args spec))]
     (doseq [[field value] (make-other spec)]
       (if (= field :fx/setup)
         (value o)
         (set-field! o field value)))
     o)))

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
