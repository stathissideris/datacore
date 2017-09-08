(ns datacore.ui.cells
  (:require [datacore.ui.java-fx :as fx]
            [datacore.ui.interactive :refer [defin] :as in]
            [datacore.cells :as c]
            [clojure.string :as str]
            [clojure.edn :as edn])
  (:import [de.jensd.fx.glyphs.fontawesome FontAwesomeIcon FontAwesomeIconView]
           [de.jensd.fx.glyphs.materialicons MaterialIcon MaterialIconView]
           [javafx.scene.paint Color]
           [javafx.scene.control Tooltip]
           [javafx.geometry Pos]))

(defn- role-icon [role roles class icon]
  (when (get roles role)
    (let [icon    (fx/make class {:fx/args [icon] :style-class "role-icon"})
          tooltip (Tooltip. (str/upper-case (name role)))] ;;javafx has this very weird bug that results
                                                           ;;in lowercase "transform" and "source" being blank in the tooltip
      (Tooltip/install icon tooltip)
      icon)))

(defn role-icons [roles]
  [(role-icon :view roles FontAwesomeIconView FontAwesomeIcon/EYE)
   (role-icon :system roles FontAwesomeIconView FontAwesomeIcon/CIRCLE_THIN)
   (role-icon :control roles FontAwesomeIconView FontAwesomeIcon/USER_ALT)
   (role-icon :source roles MaterialIconView MaterialIcon/INPUT)
   (role-icon :transform roles MaterialIconView MaterialIcon/TRANSFORM)])

;;interactive

(defin add-transform-cell
  {:alias :cells/add-transform-cell
   :params [[:cell ::in/cell]
            [:code {:type   ::in/clojure-code
                    :title  "Transform cell code"
                    :prompt "Enter a Clojure expression"}]]}
  [{:keys [cell code]}]
  (let [code           `(fn [{:keys [~'data] :as ~'input}]
                          (assoc ~'input :data ~(edn/read-string code)))
        transform-cell (c/formula (eval code) ::c/unlinked
                                  {:label :transform-cell
                                   :meta {:roles #{:transform}
                                          :code  code}})
        upstream       (first (c/sources cell))]
    (c/linear-insert! upstream transform-cell cell)))

;;(filter (fn [e] (= "Documentary" (:title-type e))) data)
