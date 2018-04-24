(ns datacore.main
  (:require [rum.core :as rum]
            [cljsjs.vega-embed :as vg]
            [datacore.state :as state]
            [datacore.keys :as keys]))

(rum/defc label [text]
  [:div {:class "label"} text])

(rum/defc counter < rum/reactive []
  [:div {:on-click (fn [_] (swap! state/state update :click-count inc))}
   "Cliiiicks: " (:click-count (rum/react state/state))])

(rum/defc keyboard-info < rum/reactive []
  [:div "key-code: " (some-> state/state rum/react :keyboard pr-str)])

(rum/defc ui < rum/reactive []
  [:div
   (keyboard-info)
   [:div#overlay {:style {:display (if (= {:code "x", :key "x", :meta true}
                                          (some-> state/state rum/react :keyboard))
                                     "block" "none")}}
    [:div.modal
     [:p "prompt"]]]
   [:div
    [:div.pane.upper
     [:p "start"]
     (map (fn [x] [:p (str x)]) (range 50))
     [:p "end"]]
    [:div.pane.lower
     [:p "start"]
     (map (fn [x] [:p (str x)]) (range 50))
     [:p "end"]]]])

(set! (.-onkeydown js/window) keys/handle-key)

(rum/mount (ui) js/document.body)
