(ns datacore.ui.keys.defaults
  (:require [datacore.ui.keys :as keys]
            [datacore.util :as util]))

(def root-keymap
  (util/deep-merge
   (keys/code-map)
   {#{:ctrl :x}
    {:2 :split-window-below
     :3 :split-window-right}}))
