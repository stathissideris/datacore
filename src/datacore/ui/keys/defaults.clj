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
   {#{:ctrl :h}
    {:k :help/describe-key
     :f :help/describe-function
     :c :help/describe-cell}}))
