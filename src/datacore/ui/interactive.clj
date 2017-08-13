(ns datacore.ui.interactive
  (:require [datacore.ui.java-fx :as fx]
            [datacore.ui.util :as ui-util]
            [datacore.cells :as c]
            [datacore.state :as state]
            [clojure.string :as str]
            [me.raynes.fs :as fs]))

(def functions {})

(defn register! [var {:keys [alias] :as options}]
  (alter-var-root
   #'functions
   assoc alias (merge {:var var} options)))

(defmacro defin [name options & rest]
  `(do
     (defn ~name ~@rest)
     (register! (resolve (quote ~name)) ~options)))

(defmulti resolve-param :type)

(defmethod resolve-param ::main-component
  [_]
  (-> state/focused-component c/value ui-util/main-component))

(defmethod resolve-param ::focus-parent
  [_]
  (c/value state/focused-component))

(defmethod resolve-param ::cell-data
  [_]
  (-> state/focused-component c/value (fx/get-field :dc/cell) c/value))

(defn expand-param [[name param]]
  (if (keyword? param)
    [name {:type param}]
    [name param]))

(defn call [match]
  (if-let [{:keys [var params]} (get functions match)]
    (do
      (if params
        (prn 'CALLED var params)
        (prn 'CALLED var))
      (let [fun (deref var)]
        (if (not-empty params)
          (let [params          (map expand-param params)
                resolved-params (reduce (fn [m [k v]] (assoc m k (resolve-param v))) {} params)]
            (prn 'PARAMS var resolved-params)
            (fun resolved-params))
          (fun))))
    ::no-function))

(defin execute-function
  {:alias  :interactive/execute-function
   :help   "Call any interactive function"
   :params [[:function {:type   ::function
                        :title  "execute-function"
                        :prompt "Select the function to execute"}]]}
  [{:keys [function]}]
  (call function))

;;;;;;;;;;;;;;;;;;;; autocomplete ;;;;;;;;;;;;;;;;;;;;

(defn- underline-match [text match]
  (cond (not text)
        nil

        (or (not match) (empty? match))
        [text]

        :else
        (let [i (str/index-of (str/lower-case text) (str/lower-case match))]
          (if-not i
            [text]
            (let [before (not-empty (subs text 0 i))
                  match  (subs text i (+ i (.length match)))
                  after  (not-empty (subs text (+ i (.length match))))]
              (->> (concat [before] [[:u match]] (underline-match after match))
                   (remove nil?)
                   (vec)))))))

(defn- function-item [name input]
  {:text  (underline-match (-> name str (subs 1)) input)
   :raw   (-> name str (subs 1))
   :value name})

(defn function-autocomplete [input]
  (let [input (-> input str/trim str/lower-case)]
    (if (empty? input)
      (->> functions
           keys
           (map #(function-item % input))
           (sort-by :raw)
           (take 50))
      (->> functions
           keys
           (map #(function-item % input))
           (filter #(str/includes? (:raw %) input))
           (sort-by :raw)
           (take 50)))))

(defn validate-file [item]
  (if (fs/directory? (:value item))
    "Please select a file, not a directory"
    true))

(defn- file-item [file input]
  (let [parts           (str/split (str file) #"/")
        input-last-part (some-> input (str/split #"/") last)
        text            (str (if (= 2 (count parts)) "/" ".../")
                             (last parts)
                             (when (fs/directory? file) "/"))]
    {:text  (underline-match text input-last-part)
     :raw   (str file (when (fs/directory? file) "/"))
     :value (str file)}))

(defn file-autocomplete [input]
  (let [input (-> input str/trim str/lower-case)]
    (cond (empty? input)
          []

          (and (str/ends-with? input "/")
               (fs/directory? input))
          (map #(file-item % input) (fs/list-dir input))

          (and (not (fs/exists? input))
               (fs/exists? (fs/parent input)))
          (let [last-part (last (str/split input #"/"))]
            (->> (file-autocomplete (str (fs/parent input) "/"))
                 (filter #(str/includes?
                           (-> % :raw str/lower-case)
                           last-part))
                 (map #(assoc % :text (underline-match (-> % :text first) last-part)))))

          (fs/exists? input)
          [(file-item (-> input fs/file .getCanonicalFile str) input)])))
