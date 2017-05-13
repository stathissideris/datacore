(ns datacore.ui
  (:require [datacore.ui.interactive :as interactive :refer [defin]]
            [datacore.ui.message :as message]
            [datacore.ui.java-fx :as fx]))

(defin execute-command
  {:alias :execute-command
   :params [{:type :text}]}
  [function-name]
  (let [alias (keyword function-name)]
    (if-let [{:keys [var]} (get interactive/functions alias)]
      (let [fun (deref var)]
        (fun))
      (message/error (str "No interactive function with alias " alias " found!")))))

;;to see live CSS updates:

(comment
  (add-stylesheet @scene "file:///tmp/default.css"))
