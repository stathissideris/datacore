(ns datacore.keys
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(def rename-key
  {:alt-down? :alt
   :control-down? :ctrl
   :meta-down? :meta
   :shift-down? :shift
   :shortcut-down? :shortcut})

(def sequence-state (atom []))

(def root-keymap
  {[#{:ctrl :x} :2] :split-window-below
   [#{:ctrl :x} :3] :split-window-right})

(defn event->combo [event]
  (let [event     (some-> event :fn-fx/includes :fn-fx/event)
        code-name (some-> event :code .getName str/lower-case keyword)
        combo     (reduce-kv
                   (fn [combo key down]
                     (if (= true down) ;;true? does not work
                       (conj combo (rename-key key)) combo))
                   #{code-name} event)]
    (if (= 1 (count combo))
      (first combo)
      combo)))

(defn global-key-handler [{:keys [event] :as all-data}]
  (try
   (when (#{:key-pressed :key-released} event)
     (let [code      (some-> all-data :fn-fx/includes :fn-fx/event :code)
           modifier? (.isModifierKey code)]
       ;;(prn 'KEY (assoc @modifiers-state :code code-name :event event))
       (when (= :key-released event)
         (let [combo (event->combo all-data)]
           (when-not modifier?
             (prn 'KEY combo)
             (swap! sequence-state conj {:time (System/currentTimeMillis) :combo combo}))
           (when-let [match (get root-keymap (mapv :combo @sequence-state))]
             (prn 'COMBO match)
             (reset! sequence-state []))))))
   (catch Exception e
     (.printStackTrace e))))
