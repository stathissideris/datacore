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
    #{:ctrl :h}
    {:k :help/describe-key
     :f :help/describe-function
     :c :help/describe-cell}}})

(def table-keymap
  {:name :datacore.ui.view/table
   :mapping
   {#{:ctrl :x} {:1 :table/scroll-to-top}}})

(c/defcell keymaps {:root                   root-keymap
                    :datacore.ui.view/table table-keymap})
