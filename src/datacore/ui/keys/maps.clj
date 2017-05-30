(ns datacore.ui.keys.maps
  (:refer-clojure :exclude [merge])
  (:require [datacore.util :as util]
            [datacore.cells :as c]
            [clojure.string :as str]
            [clojure.math.combinatorics :as combo])
  (:import [javafx.scene.input KeyEvent KeyCode]))

(def modifiers [:alt :ctrl :meta :shift :shortcut])

(defn- keycode->keyword [k]
  (-> k .getName str/lower-case (str/replace " " "-") (str/replace "/" "-") keyword))

(defn code-map []
  (into
   {}
   (for [keycode (map keycode->keyword (into [] (KeyCode/values)))
         mod (map set (combo/subsets modifiers))]
     (let [combo (conj mod keycode)
           combo (if (= 1 (count combo)) (first combo) combo)]
       [combo ::propagate]))))

(defn merge [a b]
  (if-not b
    a
    {:name [(:name a) (:name b)]
     :mapping
     (util/deep-merge (:mapping a) (:mapping b))}))

(def root-keymap
  {:name :root
   :mapping
   {#{:ctrl :x}
    {:1                :windows/maximize
     :2                :windows/split-below
     :3                :windows/split-right
     :0                :windows/delete
     #{:shift :equals} :windows/balance
     #{:ctrl :f}       :open-file}
    :f1
    {:up    :windows/focus-up
     :down  :windows/focus-down
     :left  :windows/focus-left
     :right :windows/focus-right
     :n     :windows/new}
    :f2
    {:up    :windows/swap-up
     :down  :windows/swap-down
     :left  :windows/swap-left
     :right :windows/swap-right}
    #{:meta :shortcut :x} :interactive/execute-function
    #{:ctrl :h}
    {:k :help/describe-key
     :f :help/describe-function
     :c :help/describe-cell}}})

(def prompt-keymap
  {:name :datacore.ui.view/prompt
   :mapping
   {:up         :prompt/prev-suggestion
    :down       :prompt/next-suggestion
    #{:ctrl :p} :prompt/prev-suggestion
    #{:ctrl :n} :prompt/next-suggestion
    :tab        :prompt/complete
    :enter      :prompt/accept
    #{:ctrl :g} :prompt/cancel
    #{:ctrl :a} :prompt/home
    #{:ctrl :e} :prompt/end
    #{:ctrl :b} :prompt/backward-char
    #{:ctrl :f} :prompt/forward-char
    #{:ctrl :d} :prompt/delete-forward-char}})

(def table-keymap
  {:name :datacore.ui.view/table
   :mapping
   {:esc        {#{:shift :comma}  :table/scroll-to-top
                 #{:shift :period} :table/scroll-to-bottom}
    #{:ctrl :a} :table/scroll-to-first-column
    #{:ctrl :e} :table/scroll-to-last-column}})

(c/defcell keymaps {:root                    root-keymap
                    :datacore.ui.view/prompt prompt-keymap
                    :datacore.ui.view/table  table-keymap})

(defn keys-for-function [{:keys [mapping] :as keymap} function]
  (->> (util/flatten-keys mapping)
       (filter #(= function (second %)))
       (map first)))

(defn keys-for-function-in-keymaps [keymaps function]
  (apply concat
   (for [{:keys [name] :as keymap} (vals keymaps)]
     (map #(vector name %) (keys-for-function keymap function)))))

(defn set-key!
  ([key-sequence function-alias]
   (set-key! :root key-sequence function-alias))
  ([keymap-name key-sequence function-alias]
   (c/swap! keymaps assoc-in
            (concat [keymap-name :mapping]
                    key-sequence)
            function-alias)))
