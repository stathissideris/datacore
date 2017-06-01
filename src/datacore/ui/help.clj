(ns datacore.ui.help
  (:require [datacore.ui.interactive :as in :refer [defin]]
            [datacore.ui.view.web :as web]
            [datacore.cells :as c]
            [datacore.ui.keys.maps :as keymaps]
            [datacore.ui.view :as view]
            [datacore.ui.java-fx :as fx]
            [datacore.ui.windows :as windows]
            [datacore.util :as util]
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

(defn- function-help [{:keys [alias help params] :as fun} keymaps]
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
            [:li
             [:span {:class "param-name"} (name param-name)]
             (when (:help param) " - ")
             (:help param)])]])
      (key-help fun keymaps)]])})

;; try: (keymaps/set-key! [#{:meta :shortcut :q}] :interactive/execute-function)
;; to see help be updated live!

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
