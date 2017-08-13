(ns datacore.ui.help
  (:require [datacore.ui.interactive :as in :refer [defin]]
            [datacore.ui.view.web :as web]
            [datacore.cells :as c]
            [datacore.ui.keys.maps :as keymaps]
            [datacore.ui.view :as view]
            [datacore.ui.java-fx :as fx]
            [datacore.ui.windows :as windows]
            [datacore.util :as util]
            [datacore.ui.util :as ui.util]
            [hiccup.core :refer [html]]
            [clojure.string :as str]))

(defn- friendly-key [key]
  [:span
   {:class "key-press"}
   (if (keyword? key)
     (name key)
     (->> (disj key :meta :ctrl :shortcut :shift)
          (map name)
          (concat
           (remove nil?
                   [(when (:ctrl key) "ctrl")
                    (when (:shift key) "shift")
                    (when (and (:meta key) (:shortcut key)) "command")
                    ;;TODO option
                    ]))
          (str/join "+")))])

(defn- friendly-key-sequence [key-seq]
  [:span {:class "key-sequence"} (interpose "," (remove nil? (map friendly-key key-seq)))])

(defn- key-help [{:keys [alias]} keymaps]
  (when-let [key-seqs (not-empty (keymaps/keys-for-function-in-keymaps keymaps alias))]
    [:div
     [:h2 "keys"]
     [:p (if (== 1 (count key-seqs))
           "You can run this function using the following key sequence:"
           "You can run this function using the following key sequences:")]
     [:ul
      (for [[keymap sequence] key-seqs]
        [:li (friendly-key-sequence sequence)
         [:span
          {:class "keymap"}
          "keymap: " [:span {:class "keymap-name"} (util/kw-str keymap)]]])]]))

(defn- normalise-param [param]
  (if (keyword? param)
    {:dispatch param}
    param))

(defn- function-help [{:keys [alias help related params] :as fun} keymaps]
  {:title (str "Help for function " (util/kw-str alias))
   :content
   (html
    [:html
     [:body
      {:class "dc-help"}
      [:h1 {:class "function-name"} (util/kw-str alias)]
      [:p (or help "(undocumented)")]
      (when params
        [:div
         [:h2 "parameters"]
         [:ul
          (for [[param-name param] params]
            (let [{:keys [dispatch help]} (normalise-param param)]
              [:li
               [:span {:class "param-name"} (name param-name)]
               " - "
               (or help (in/resolve-param-help dispatch))
               " (" [:span {:class "resolve-param-dispatch"} (str dispatch)] ")"]))]])
      (key-help fun keymaps)
      (when related
        [:div
         [:h3 "see also"]
         [:p
          (interpose
           [:span {:class "pre"} ", "]
           (for [r related]
             ;;[:a {:href (str "function:" r)} [:span {:class "function-name"} (subs (str r) 1)]]
             [:span {:class "function-name"} [:a {:href (str "function:" r)} (subs (str r) 1)]]))]])]])})

;; try: (keymaps/set-key! [#{:meta :shortcut :q}] :interactive/execute-function)
;; to see help be updated live!

(defn- url-protocol [url]
  (second (re-find #"^(.+?):" url)))

(defn- url-function [url]
  (keyword (last (re-find #"^(.+?)::(.+?)$" url))))

(defin describe-function
  {:alias :help/describe-function
   :params [[:function {:type   ::in/function
                        :title  "describe-function"
                        :prompt "Show help for interactive function"}]]}
  [{:keys [function]}]
  (let [help-content (c/formula
                      (fn [functions keymaps]
                        (println (str "Help for function \"" (name function) "\""))
                        (-> (function-help (get functions function) keymaps)
                            (assoc :link-listener
                                   (fn [event href type]
                                     (.preventDefault event)
                                     (when (= type :click)
                                       (if (= "function" (url-protocol href))
                                         (describe-function {:function (url-function href)})
                                         (fx/open-in-browser href)))))))
                      in/functions
                      keymaps/keymaps
                      {:label :help-content})
        _            (c/alter-meta! help-content assoc :roles #{:source})
        view-cell    (web/view help-content)
        component    (view/configure-view
                      {:cell      view-cell
                       :component (view/build-cell-view view-cell)
                       :focused?  true})]
    (fx/run-later!
     #(windows/replace-focused! component))))
