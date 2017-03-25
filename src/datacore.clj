(ns datacore)

(def interactive-functions {})

(defn register-interactive [var {:keys [alias] :as options}]
  (alter-var-root
   #'interactive-functions
   assoc alias {:var var}))

(defmacro defin [name options & rest]
  `(do
     (defn ~name ~@rest)
     (register-interactive (resolve (quote ~name)) ~options)))
