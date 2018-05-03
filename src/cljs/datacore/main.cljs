(ns datacore.main
  (:require [rum.core :as rum :refer [defc]]
            [cljsjs.vega-embed]
            [datacore.state :as state]
            [datacore.keys :as keys]
            [taoensso.sente]
            [mount.core :as mount])
  (:require-macros [mount.core :refer [defstate]]
                   [cljs.core.async.macros :as a :refer [go go-loop]]))

(enable-console-print!)

(defstate sente :start (taoensso.sente/make-channel-socket! "/chsk" {:type :auto}))

(add-watch (:state @sente) :sente
           (fn [_ _ _ new-state]
             (swap! state/state assoc-in [:sente :open?] (:open? new-state))))

;;how to handle events: https://github.com/danielsz/system-websockets/blob/master/src/cljs/demo/core.cljs#L46-L56

(go-loop []
  (let [{:as msg :keys [event]} (<! (:ch-recv @sente))]
    ;;(println (pr-str event))
    (when (= :chsk/recv (first event))
      (let [payload (second event)]
        (swap! state/state assoc-in [:sente :latest] payload)
        (when (= :mouse/move (first payload))
          (swap! state/state assoc :mouse (second payload)))))
    (recur)))

(defn- dom-node [state]
  (some-> state :rum/react-component js/ReactDOM.findDOMNode))

(defn- vega-mount [state]
  (js/vegaEmbed (dom-node state)
                (some-> state :rum/args first clj->js)
                (some-> state :rum/args second clj->js))
  state)

(defc vega < {:did-mount   vega-mount
              :will-update vega-mount}
  [spec options]
  [:div])

(defc label [text]
  [:div {:class "label"} text])

(defc counter < rum/reactive []
  [:div {:on-click (fn [_] (swap! state/state update :click-count inc))}
   "Cliiiicks: " (:click-count (rum/react state/state))])

(defc keyboard-info < rum/static [keyboard]
  [:div#footer "key-code: " (pr-str keyboard)])

(defc ui < rum/reactive []
  (let [{:keys [sente mouse keyboard] :as state} (rum/react state/state)]
   [:div#top
    [:div "sente connected: " (some-> sente :open? pr-str)]
    [:div "sente latest payload: " (some-> sente :latest pr-str)]
    [:div "mouse: " (pr-str mouse)]
    (let [{:keys [x y]} mouse]
      [:div {:style {:top      (str (- y 10) "px")
                     :left     (str (- x 5) "px")
                     :position "absolute"}}
       [:img {:style {:width "40px" :height "auto"} :src "img/mouse-pointer.png"}]])
    ;;[:div "state: " (some-> state/state rum/react pr-str)]
    (counter)
    (vega {:$schema     "https//vega.github.io/schema/vega-lite/v2.0.json"
           :description "A simple bar chart with embedded data."
           :data        {:values [{:a "A test" :b 28}
                                  {:a "B test" :b 55}
                                  {:a "C test" :b 43}
                                  {:a "D test" :b 91}
                                  {:a "E test" :b 81}
                                  {:a "F test" :b 53}
                                  {:a "G test" :b (:click-count (rum/react state/state))}
                                  {:a "H test" :b 87}
                                  {:a "I test" :b 52}]}
           :mark        "bar"
           :encoding    {:x {:field "a"
                             :type  "ordinal"}
                         :y {:field "b"
                             :type  "quantitative"}}}
          {:renderer "svg"
           :theme    "quartz"
           :actions  false})
    [:div#overlay {:style {:display (if (= keyboard {:code "x", :key "x", :meta true})
                                      "block" "none")}}
     [:div.modal
      [:p "prompt"]]]
    (comment
      [:dev#panes
       [:div.h-split
        [:div.pane
         [:p "-start-"]
         (map (fn [x] [:p (str "A-" x)]) (range 50))
         [:p "-end-"]]
        [:div.pane
         [:p "-start-"]
         (map (fn [x] [:p (str "B-" x)]) (range 50))
         [:p "-end-"]]]
       [:div {:style {:clear "both"}}]])
    (keyboard-info keyboard)]))

(set! (.-onkeydown js/window) keys/handle-key)
(set! (.-onmousemove js/document)
      (fn [event]
        ((:send-fn @sente) [:mouse/move {:x event.pageX :y event.pageY}])))

(rum/mount (ui) (.getElementById js/document "ui"))

;;(rum/mount (ui) js/document.body)
