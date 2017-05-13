(ns datacore.ui.interactive
  (:require [datacore.ui.java-fx :as fx]
            [datacore.ui.util :as ui-util]
            [datacore.cells :as c]
            [datacore.state :as state]))

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
          (let [resolved-params (reduce-kv (fn [m k v] (assoc m k (resolve-param v))) {} params)]
            (prn 'params resolved-params)
            (fun resolved-params))
          (fun))))
    ::no-function))
