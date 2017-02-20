(ns datacore.ui.table
  (:require [datacore.ui.observable :refer [observable-list]])
  (:import [javafx.scene.control TableView TableColumn]
           [javafx.util Callback]
           [javafx.beans.property ReadOnlyObjectWrapper]))

(defn callback [fun]
  (reify Callback
    (call [this p]
      (fun p))))

(defn column [name cell-value-fn]
  (doto (TableColumn. name)
    (.setCellValueFactory
     (callback #(ReadOnlyObjectWrapper. (cell-value-fn (.getValue %)))))))

(defn set-columns! [table columns]
  (.. table getColumns (setAll columns))
  table)

(defn view [data]
  (TableView. (observable-list data)))
