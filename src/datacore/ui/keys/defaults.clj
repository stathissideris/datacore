(ns datacore.ui.keys.defaults
  (:require [datacore.ui.keys :as keys]
            [datacore.util :as util]))

(def root-keymap
  (util/deep-merge
   (keys/code-map)
   {#{:ctrl :x}
    {:1 :windows/maximize
     :2 :windows/split-below
     :3 :windows/split-right
     :0 :windows/delete
     #{:ctrl :f} :open-file}}
   {:f1
    {:up    :windows/focus-up
     :down  :windows/focus-down
     :left  :windows/focus-left
     :right :windows/focus-right}}
   {#{:ctrl :h}
    {:k :help/describe-key
     :f :help/describe-function
     :c :help/describe-cell}}))
