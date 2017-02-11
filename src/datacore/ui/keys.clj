(ns datacore.ui.keys
  (:require [clojure.string :as str]
            [clojure.set :as set]
            [clojure.math.combinatorics :as combo]
            [datacore.ui.timer :as timer]
            [datacore.util :as util])
  (:import [javafx.scene.input KeyEvent KeyCode]))

(def chain (atom []))
(def timer (atom nil))

(def modifiers [:alt :ctrl :meta :shift :shortcut])

(defn keycode->keyword [k]
  (-> k .getName str/lower-case (str/replace " " "-") (str/replace "/" "-") keyword))

(defn code-map []
  (into
   {}
   (for [keycode (map keycode->keyword (into [] (KeyCode/values)))
         mod (map set (combo/subsets modifiers))]
     (let [combo (conj mod keycode)
           combo (if (= 1 (count combo)) (first combo) combo)]
       [combo ::propagate]))))

(def global-keymap
  (util/deep-merge
   (code-map)
   {#{:ctrl :x}
    {:2 :split-window-below
     :3 :split-window-right}}))

(defn prefix? [x] (map? x))
(defn action? [x] (keyword? x))

(defn event->map [fx-event]
  {:type      (condp = (.getEventType fx-event)
                KeyEvent/KEY_PRESSED :key-pressed
                KeyEvent/KEY_TYPED :key-typed
                KeyEvent/KEY_RELEASED :key-released)
   :code      (-> fx-event .getCode .getName str/lower-case keyword)
   :alt       (.isAltDown fx-event)
   :ctrl      (.isControlDown fx-event)
   :meta      (.isMetaDown fx-event)
   :shift     (.isShiftDown fx-event)
   :shortcut  (.isShortcutDown fx-event)
   :modifier? (-> fx-event .getCode .isModifierKey)})

(defn event->press [{:keys [code alt ctrl meta shift shortcut]}]
  (let [press (->> [code
                    (when alt :alt)
                    (when ctrl :ctrl)
                    (when meta :meta)
                    (when shift :shift)
                    (when shortcut :shortcut)]
                   (remove nil?)
                   set)]
    (if (= 1 (count press))
      (first press)
      press)))

(defn- clear-chain! []
  (reset! chain [])
  nil)

(defn- wait-for-next! []
  (timer/cancel @timer)
  (reset! timer (timer/delayed 3500 #(do (prn 'KEY-STATE-RESET) (clear-chain!)))))

(defn global-key-handler [fx-event]
  (try
    (let [{:keys [modifier? type] :as event} (event->map fx-event)]
      (if (not= :key-pressed type)
        (do
          (prn 'CONSUMED event)
          (.consume event)
          ::consume)
        (when (= :key-pressed type)
          (let [press (event->press event)]
            (when-not modifier?
              (swap! chain conj press)
              (wait-for-next!)
              (prn 'KEY press 'SEQ @chain))
            (let [match (get-in global-keymap @chain)]
              (cond
                (not match)
                (do
                  (println (str "ERROR - Key sequence " (pr-str @chain) " not mapped to anything"))
                  (clear-chain!))

                (= match ::propagate)
                (do
                  (prn 'PROPAGATED event)
                  (clear-chain!))

                :else
                (do
                  (when (action? match)
                    (prn 'COMBO match)
                    (timer/cancel @timer)
                    (clear-chain!))
                  (when (prefix? match) (prn 'PREFIX @chain))
                  (prn 'CONSUMED event)
                  (.consume event)
                  ::consume)))))))
    (catch Exception e
      (.printStackTrace e))))
