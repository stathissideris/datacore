(ns datacore.fx.utils
  (:require [clojure.string :as str])
  (:import [java.lang.reflect Constructor Method Parameter Modifier Field]))

(defn kebab->class [from]
  (let [s (str/split (name from) #"\-")]
    (apply str (map str/capitalize s))))

(defn get-getter [^Class klass prop]
  (let [prop-name      (str "get" (kebab->class (name prop)))
        ^Method method (->> (.getMethods klass)
                            (filter #(= prop-name (.getName ^Method %)))
                            (filter #(zero? (count (.getParameters ^Method %))))
                            first)
        arr            (make-array Object 0)]
    (.setAccessible method true)
    (fn [inst]
      (.invoke method inst arr))))

(defn get-setter [^Class klass prop]
  (let [prop-name      (str "set" (kebab->class (name prop)))
        ^Method method (->> (.getMethods klass)
                            (filter #(= prop-name (.getName ^Method %)))
                            (filter #(= 1 (count (.getParameters ^Method %))))
                            first)
        _              (assert method (str "No property " prop " on type " klass))
        to-type        (.getType ^Parameter (first (.getParameters method)))]
    (.setAccessible method true)
    (fn [inst val]
      (let [^objects arr (make-array Object 1)]
        (aset arr 0 val) ;;convert this val
        (.invoke method inst arr)))))

(defn make
  ([tp args props]
   (make tp args props))
  ([tp args props calls]
   ))
