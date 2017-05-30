(ns datacore.ui.help
  (:require [datacore.ui.interactive :as in :refer [defin]]
            [datacore.ui.view.web :as web]
            [datacore.cells :as c]
            [datacore.ui.keys.maps :as keymaps]
            [datacore.ui.view :as view]
            [datacore.ui.java-fx :as fx]
            [datacore.ui.windows :as windows]
            [datacore.util :as util]
            [hiccup.core :refer [html]]))

(defn- key-help [{:keys [alias]} keymaps]
  (when-let [key-combos (not-empty (keymaps/keys-for-function-in-keymaps keymaps alias))]
    [:div
     [:h2 "keys"]
     [:p "You can run this function using the following key sequences:"]
     [:ul
      (for [[keymap sequence] key-combos]
        [:li [:tt (pr-str sequence)] " (in keymap " [:tt (util/kw-str keymap)] ")"])]]))

(defn- function-help [{:keys [alias help params] :as fun} keymaps]
  {:content
   (html
    [:html
     [:body
      [:h1 (-> alias str (subs 1))]
      [:p help]
      (when params
        [:div
         [:h2 "parameters"]
         [:ul
          (for [[param-name param] params]
            [:li
             [:tt (name param-name)]
             (when (:help param) " - ")
             (:help param)])]])
      (key-help fun keymaps)]])})

(defin describe-function
  {:alias :help/describe-function
   :params [[:function {:type   ::in/function
                        :title  "describe-function"
                        :prompt "Show help for interactive function"}]]}
  [{:keys [function]}]
  (when-let [fun (get in/functions function)]
    (println (str "Help for function \"" (name function) "\": " (:help fun)))

    (let [help-content (c/formula
                        (fn [keymaps]
                          (function-help fun keymaps))
                        keymaps/keymaps
                        {:label :help-content})
          help-view    (web/view help-content)
          component    (view/build-view
                        {:type       :datacore.ui.view/cell
                         :cell       help-view
                         :focused?   true
                         :focusable? true})]
      (fx/run-later!
       #(windows/replace-focused! component)))))
