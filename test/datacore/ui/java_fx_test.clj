(ns datacore.ui.java-fx-test
  (:refer-clojure :exclude [parents])
  (:require [datacore.ui.java-fx :refer :all]
            [clojure.test :refer :all]))

(init)

(deftest parents-test
  (let [foo (label "foo")
        c   (make
             :scene.layout/border-pane
             {:id     "a"
              :center (make
                       :scene.layout/border-pane
                       {:id     "b"
                        :center (make
                                 :scene.layout/border-pane
                                 {:id     "c"
                                  :center (make
                                           :scene.layout/border-pane
                                           {:id     "d"
                                            :center foo})})})})]
    (is (= ["d" "c" "b" "a"] (map #(.getId %) (parents foo))))))

(deftest has-style-class?-test
  (let [c (make :scene.control/label
                {:style-class ["foo" "bar"]
                 :text        "test"})]
    (is (true? (has-style-class? c "foo")))
    (is (true? (has-style-class? c "bar")))
    (is (false? (has-style-class? c "baz")))))

(deftest set-fields-test
  (let [l (label "foo")]
    (set-fields! l {:id          "the-id"
                    :style-class ["focusable"]})
    (is (= "the-id" (.getId l)))
    (is (= ["focusable"] (seq (.getStyleClass l))))))
