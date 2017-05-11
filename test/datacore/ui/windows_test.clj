(ns datacore.ui.windows-test
  (:require [datacore.ui.windows :refer :all]
            [datacore.main :as main]
            [datacore.state :as state]
            [clojure.spec :as s]
            [clojure.spec.gen :as gen]
            [clojure.test :refer :all]
            [clojure.test.check.properties :as prop]
            [clojure.test.check :as tc]))

;; (def fn-mapping
;;   {:split-below split-below
;;    :split-right split-right
;;    :focus-left  focus-left
;;    :focus-right focus-right
;;    :focus-up    focus-up
;;    :focus-down  focus-down
;;    :swap-left   swap-left
;;    :swap-right  swap-right
;;    :swap-up     swap-up
;;    :swap-down   swap-down})
;;
;; (s/def ::script (s/+ #{:split-below :split-right
;;                        :focus-left :focus-right :focus-up :focus-down
;;                        :swap-left :swap-right :swap-up :swap-down
;;                        }))
;;
;; (defn- run-script
;;   ([script]
;;    (run-script script 300)) ;;TODO magic number, any quicker fails - dunno if javafx or some problem with cells
;;   ([script sleep-interval]
;;    (reset! main/uncaught-exception nil)
;;    (prn '---)
;;    (main/init)
;;    (doseq [command script]
;;      (prn command)
;;      (Thread/sleep sleep-interval)
;;      ((fn-mapping command)))
;;    (state/swap-layout! update :children (comp vec butlast))))
;;
;; (def window-operations-dont-crash-prop
;;   (prop/for-all
;;    [script (s/gen ::script)]
;;    (do
;;      (run-script script)
;;      (nil? @main/uncaught-exception))))

#_(tc/quick-check 30 window-operations-dont-crash-prop)
;;:smallest [(:split-right :swap-right :swap-right)]
;;[:split-right :split-below :split-below :swap-right :swap-down :focus-down :split-right]
;;[:split-below :split-right :swap-right]
