(ns datacore.view
  (:require [datacore.ui.message :as message]
            [datacore.cells :as c]))

(defmulti build-view (comp :type c/value))
