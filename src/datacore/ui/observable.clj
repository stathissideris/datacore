(ns datacore.ui.observable
  (:require [datacore.util :as util]
            [dev :as dev])
  (:import [javafx.collections FXCollections ObservableList ListChangeListener ListChangeListener$Change]))

(defmulti observable-list type)

(defmethod observable-list clojure.lang.APersistentVector
  [^java.util.List x]
  (FXCollections/observableArrayList x))

(defn list-change [the-list old new]
  (let [diff     (->> (util/seq-diff old new)
                      (remove #(-> % first (= :same)))
                      (partition-by first))
        idx      (atom -1)
        current  #(nth diff @idx)]
    (dev/trace-proxy
     (proxy [ListChangeListener$Change] [the-list]
       (getList [] the-list)

       (next [] (swap! idx inc) (< @idx (count diff)))
       (reset [] (reset! idx 0))

       (wasAdded [] (= :add (ffirst (current))))
       (wasRemoved [] (= :delete (ffirst (current))))
       (wasReplaced [] (= :edit (ffirst (current))))
       (wasUpdated [] (= :edit (ffirst (current))))
       (wasPermutated [] false)

       (getAddedSize [] (if-not (.wasAdded this) 0 (count (current))))
       (getAddedSubList [] (map second (current)))

       (getFrom [] (apply + (map count (take @idx diff))))
       (getTo [] (+ (apply + (map count (take @idx diff)))
                    (count (current))))

       #_(getPermutation
           ([] (prn 'CALLED 'getPermutation [this]))
           ([int] (prn 'CALLED 'getPermutation [this int])))

       (getRemoved [] (map second (current)))
       (getRemovedSize [] (if-not (.wasRemoved this) 0 (count (current))))))))

(defmethod observable-list clojure.lang.Atom
  [x]
  (proxy [Object ObservableList]
      []
    (get [i]
      (nth @x i))
    (size []
      (count @x))
    (isEmpty []
      (empty? @x))
    (setAll [coll]
      (reset! x coll))
    (addListener [listener]
      (prn 'ADDING-LISTENER listener)
      (add-watch x :observable-list-atom-listener
                 (fn [k ref old new]
                   (prn 'ATOM-CHANGED)
                   (.onChanged listener (list-change this old new)))))))
