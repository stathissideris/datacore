(ns datacore.ui.interactive
  (:require [datacore.ui.java-fx :as fx]
            [datacore.ui.util :as ui-util]
            [datacore.cells :as c]
            [datacore.state :as state]
            [clojure.string :as str]
            [me.raynes.fs :as fs]))

(def functions {})

(defn register! [var {:keys [alias] :as options}]
  (alter-var-root
   #'functions
   assoc alias (merge {:var var} options)))

(defmacro defin [name options & rest]
  `(do
     (defn ~name ~@rest)
     (register! (resolve (quote ~name)) ~options)))

(defmulti resolve-param identity)

(defmethod resolve-param ::main-component
  [_]
  (-> state/focused-component c/value ui-util/main-component))

(defn call [match]
  (if-let [{:keys [var params]} (get functions match)]
    (do
      (prn var params)
      (let [fun (deref var)]
        (if (not-empty params)
          (let [resolved-params (reduce (fn [m [k v]] (assoc m k (resolve-param v))) {} params)]
            (prn 'params resolved-params)
            (fun resolved-params))
          (fun))))
    ::no-function))

(defn- wrap-function [name]
  {:text  (-> name str (subs 1))
   :value name})

(defn function-autocomplete [input]
  (let [input (-> input str/trim str/lower-case)]
    (if (empty? input)
      (->> functions
           keys
           (map wrap-function)
           (sort-by :text)
           (take 50))
      (->> functions
           keys
           (map wrap-function)
           (filter #(str/includes? (:text %) input))
           (sort-by :text)
           (take 50)))))

(defn- wrap-filename [file]
  (let [parts (str/split (str file) #"/")]
    {:text  (str (if (= 2 (count parts)) "/" ".../")
                 (last parts)
                 (when (fs/directory? file) "/"))
     :value (str file)}))

(defn file-autocomplete [input]
  (let [input (-> input str/trim str/lower-case)]
    (cond (empty? input)
          []

          (and (str/ends-with? input "/")
               (fs/directory? input))
          (map wrap-filename (fs/list-dir input))

          (and (not (fs/exists? input))
               (fs/exists? (fs/parent input)))
          (let [last-part (last (str/split input #"/"))]
            (->> (file-autocomplete (str (fs/parent input) "/"))
                 (filter #(str/includes?
                           (-> % :text str/lower-case)
                           last-part))))

          (fs/exists? input)
          [(wrap-filename input)])))
