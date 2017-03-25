(ns datacore.ui.windows
  (:require [datacore :refer [defin]]))

(defin maximize
  {:alias :windows/maximize}
  []
  (println :maximize))

(defin split-below
  {:alias :windows/split-below}
  []
  (println :below))

(defin split-right
  {:alias :windows/split-right}
  []
  (println :right))

(defin delete
  {:alias :windows/delete}
  []
  (println :delete))
