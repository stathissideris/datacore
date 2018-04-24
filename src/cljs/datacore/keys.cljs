(ns datacore.keys
  (:require [clojure.string :as str]
            [camel-snake-kebab.core :as csk]
            [datacore.state :as state]))

(def modifier? #{"meta-left" "shift-left" "control-left" "alt-left"
                 "meta-right" "shift-right" "control-right" "alt-right"})

(defn handle-key [e]
  (let [m (reduce-kv
           (fn [m k v]
             (if v (assoc m k v) m))
           {}
           {:code  (-> (.-code e) (str/replace "Key" "") csk/->kebab-case str/lower-case)
            :key   (.-key e)
            :ctrl  (.-ctrlKey e)
            :meta  (.-metaKey e)
            :alt   (.-altKey e)
            :shift (.-shiftKey e)})]
    (when-not (modifier? (:code m))
      (swap! state/state assoc :keyboard m)))
  (.log js/console (clj->js e)))
