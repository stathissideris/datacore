(ns datacore.ui
  (:require [datacore.ui.interactive :as interactive :refer [defin]]
            [datacore.ui.message :as message]
            [datacore.ui.java-fx :as fx]))

;;to see live CSS updates:

(comment
  (add-stylesheet @scene "file:///tmp/default.css"))
