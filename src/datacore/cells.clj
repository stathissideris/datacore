(ns datacore.cells
  (:refer-clojure :exclude [swap!]))

(def id (ref 0))
(def cells (ref {}))
(def links (ref {}))

(defn link! [cell1 cell2]
  (dosync
   (alter links update (.-id cell2)
          (fn [x] (if-not x #{(.-id cell1)} (conj x (.-id cell1)))))))

(def ^:dynamic *cell-context* nil)

(defn value [cell]
  (when *cell-context*
    (link! *cell-context* cell))
  (if (.-formula cell)
    (binding [*cell-context* cell]
      ((get @cells (.-id cell))))
    (get @cells (.-id cell))))

(deftype CellID [id formula]
  clojure.lang.IRef
  (deref [this] (value this)))

(defn cell? [x] (instance? CellID x))

(defn- register-cell! [x formula?]
  (dosync
   (let [current-id @id]
     (alter id inc)
     (alter cells assoc current-id x)
     (CellID. current-id formula?))))

(defn cell [x]
  (register-cell! x false))

(defn formula [fun]
  (register-cell! fun true))

(defmacro cell= [& code]
  `(formula (fn [] ~@code)))

(defn swap! [cell fun & args]
  (if (.-formula cell)
    (throw (ex-info "Cannot swap, cell is a formula" {:cell cell}))
    (dosync
     (let [current @cell]
       (alter cells assoc (.-id cell) (apply fun current args))
       (doseq [linked (get @links (.-id cell))]
         ((get @cells linked)))
       @cell))))

(comment
  (def foo (cell 100))
  (def bar (cell 2))
  (def baz (cell= (prn "calc baz!" (* 2 @foo @bar))
                  (* 2 @foo @bar)))
  ;;or
  (def baz (formula #(do
                       (prn "calc baz!" (* 2 @foo @bar))
                       (* 2 @foo @bar))))
  @baz

  (swap! foo inc)
  (swap! bar inc)
  )
