(ns datacore.ui.keys
  (:require [clojure.string :as str]
            [clojure.set :as set]
            [clojure.math.combinatorics :as combo]
            [datacore.ui.timer :as timer]
            [datacore.util :as util])
  (:import [javafx.scene.input KeyEvent KeyCode]
           [javafx.event Event]))

(def chain (ref []))
(def timer (ref nil))
(def last-consumed (ref nil))

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
   :code      (if (= KeyEvent/KEY_TYPED (.getEventType fx-event))
                (-> fx-event .getCharacter keyword)
                (-> fx-event .getCode .getName str/lower-case keyword))
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

(defn- clear-chain []
  (timer/cancel @timer)
  (alter last-consumed (constantly nil))
  (alter chain (constantly []))
  nil)

(defn- wait-for-next []
  (dosync
   (timer/cancel @timer)
   (alter timer (constantly (timer/delayed 3500 #(do (prn 'KEY-STATE-RESET) (dosync (clear-chain))))))))

;;(def debug prn)
(defn debug [& _])

(defn consume-event [^Event e press event]
  (debug 'CONSUMED press event)
  (alter last-consumed (constantly event))
  (.consume e))

(defn also-consume-this? [event]
  (when-let [last-consumed @last-consumed]
    (debug 'last-consumed last-consumed)
    (= (dissoc event :type)
       (dissoc last-consumed :type))))

(defn global-key-handler [fx-event]
  (try
    (let [{:keys [modifier? type code] :as event} (event->map fx-event)
          press                                   (event->press event)
          new-chain                               (conj @chain press)
          match                                   (get-in global-keymap new-chain)]
      (cond
        ;;also consume :key-typed and :key-released equivalents of
        ;;events that have been consumed:
        (also-consume-this? event)
        (do
          (debug 'ALSO-CONSUMING)
          (dosync
           (consume-event fx-event press event)
           (when (= type :key-released)
             (alter last-consumed (constantly nil))))) ;;...but stop consuming at :key-released

        (and (= type :key-pressed) (not match))
        (do
          (println (str "ERROR - Key sequence " new-chain " not mapped to anything"))
          (dosync
           (clear-chain)
           (consume-event fx-event press event)))

        (= match ::propagate)
        (do
          (debug 'PROPAGATED press event)
          (dosync (clear-chain)))

        :else
        (cond
          (prefix? match)
          (dosync
            (alter chain conj press)
            (prn 'PREFIX @chain)
            (wait-for-next)
            (consume-event fx-event press event))
          (action? match)
          (dosync
            (prn 'COMBO new-chain match)
            (clear-chain)
            (consume-event fx-event press event)
            match))))
    (catch Exception e
      (.printStackTrace e))))
