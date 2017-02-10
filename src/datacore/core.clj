(ns datacore.core
  (:require [fn-fx.fx-dom :as dom]
            [fn-fx.diff :refer [component defui render should-update?]]
            [fn-fx.controls :as ui]
            [clojure.string :as str]
            [datacore.keys :as keys]))

;; The main login window component, notice the authed? parameter, this defines a function
;; we can use to construct these ui components, named "login-form"
(defui LoginWindow
  (render [this {:keys [authed?]}]
    (ui/grid-pane
      :alignment :center
      :hgap 10
      :vgap 10
      :padding (ui/insets
                 :bottom 25
                 :left 25
                 :right 25
                 :top 25)
      :children [(ui/text
                  :text "Welcome"
                  :font (ui/font
                         :family "Tahoma"
                         :weight :normal
                         :size 20)
                  :grid-pane/column-index 0
                  :grid-pane/row-index 0
                  :grid-pane/column-span 2
                  :grid-pane/row-span 1)

                 (ui/label
                  :text "User:"
                  :grid-pane/column-index 0
                  :grid-pane/row-index 1)

                 (ui/text-field
                  :id :user-name-field
                  :grid-pane/column-index 1
                  :grid-pane/row-index 1)

                 (ui/label :text "Password:"
                           :grid-pane/column-index 0
                           :grid-pane/row-index 2)

                 (ui/password-field
                  :id :password-field
                  :grid-pane/column-index 1
                  :grid-pane/row-index 2)

                 (ui/h-box
                  :spacing 10
                  :alignment :bottom-right
                  :children [(ui/button :text "Sign in"
                                        :on-action {:event :auth
                                                    :fn-fx/include {:user-name-field #{:text}
                                                                    :password-field #{:text}}})]
                  :grid-pane/column-index 1
                  :grid-pane/row-index 4)

                 (ui/text
                  :text (if authed? "Sign in was pressed" "")
                  :grid-pane/column-index 1
                  :grid-pane/row-index 6)])))

(comment
 (defmethod set-property [javafx.scene.Scene :event-filters]
   [^javafx.scene.Scene s _ val]
   (doseq [[event-type event-filter] val]
     (.addEventFilter s event-type event-filter))))

(def listener {:event :key-pressed
               :fn-fx/include {:fn-fx/event #{:code :event-type :text :alt-down? :control-down? :meta-down? :shift-down? :shortcut-down?}}})

;; Wrap our login form in a stage/scene, and create a "stage" function
(defui Stage
       (render [this args]
               (ui/stage
                 :title "JavaFX Welcome"
                 :shown true
                 :filter/scene.input.key-event.any
                 (assoc listener :fn-fx/consume?
                        (fn [event]
                          ;;(prn 'CONSUME? (-> event :fn-fx/includes :fn-fx/event :text (= "D")))
                          (-> event :fn-fx/includes :fn-fx/event :code .getName (= "D"))))
                 :scene (ui/scene
                         ;;:on-key-pressed (assoc listener :event :key-pressed)
                         ;;:on-key-released (assoc listener :event :key-released)
                         :root (login-window args)))))

(defn -main []
  (let [;; Data State holds the business logic of our app
        data-state (atom {:authed? false})

        ;; handler-fn handles events from the ui and updates the data state
        handler-fn (fn [{:keys [event] :as all-data}]
                     ;;(println "UI Event" event all-data)
                     ;;(keys/global-key-handler all-data)
                     (case event
                       :auth (swap! data-state assoc :authed? true)
                       :key-pressed (prn 'KEY all-data)
                       :else data-state))

        ;; ui-state holds the most recent state of the ui
        ui-state (agent (dom/app (stage @data-state) handler-fn))]

    ;; Every time the data-state changes, queue up an update of the UI
    (add-watch data-state :ui (fn [_ _ _ _]
                                (send ui-state
                                      (fn [old-ui]
                                        (dom/update-app old-ui (stage @data-state))))))))
