(ns datacore.ui.interactive
  (:require [datacore.ui.java-fx :as fx]
            [datacore.ui.util :as ui-util]
            [datacore.state :as state]))

(def functions {})

(defn register! [var {:keys [alias] :as options}]
  (alter-var-root
   #'functions
   assoc alias {:var var}))

(defmacro defin [name options & rest]
  `(do
     (defn ~name ~@rest)
     (register! (resolve (quote ~name)) ~options)))

(defmulti resolve-param identity)

(defmethod resolve-param ::main-component
  [_]
  (ui-util/main-component (state/focused-component)))

(defn call [match]
  (if-let [{:keys [var params]} (get functions match)]
    (do
      (prn var params)
      (let [fun (deref var)]
        (if (not-empty params)
          (fun (reduce-kv (fn [m k v] (assoc m k (resolve-param v))) {} params))
          (fun))))
    ::no-function))
