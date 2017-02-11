(ns datacore.ui.timer
  (:import [java.util Timer TimerTask]))

(defn delayed [millis fun]
  (future
    (Thread/sleep millis)
    (fun)))

(defn cancel [timer]
  (when timer
   (future-cancel timer)))
