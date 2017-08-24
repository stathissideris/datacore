(ns datacore.ui.java-fx-test
  (:refer-clojure :exclude [parents methods tree-seq])
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

(deftest get-field-in-test
  (testing "simple"
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
      (is (= "foo" (get-field-in c [:center :center :center :center :text])))))
  (testing "with indexes"
    (let [foo (label "foo")
          c   (make
               :scene.control/split-pane
               {:items
                [(make
                  :scene.control/split-pane
                  {:items
                   [(make
                     :scene.control/split-pane
                     {:items
                      [(make
                        :scene.control/split-pane
                        {:items
                         [foo]})]})]})]})]
      (is (= "foo" (get-field-in c [:items 0
                                    :items 0
                                    :items 0
                                    :items 0
                                    :text]))))))

(deftest set-field-in!-test
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
    (set-field-in! c [:center :center :center :center :text] "bar")
    (is (= "bar" (get-field-in c [:center :center :center :center :text])))))

(deftest has-style-class?-test
  (let [c (make :scene.control/label
                {:style-class ["foo" "bar"]
                 :text        "test"})]
    (is (true? (has-style-class? c "foo")))
    (is (true? (has-style-class? c "bar")))
    (is (false? (has-style-class? c "baz")))))

(deftest set-fields-test
  (testing "with maps"
    (let [l (label "foo")]
      (set-fields! l {:id          "the-id"
                      :style-class ["focusable"]})
      (is (= "the-id" (.getId l)))
      (is (= ["focusable"] (seq (.getStyleClass l))))))
  (testing "with vectors of vectors"
    (let [l (label "foo")]
      (set-fields! l [[:id "the-id"]
                      [:style-class ["focusable"]]])
      (is (= "the-id" (.getId l)))
      (is (= ["focusable"] (seq (.getStyleClass l)))))))

(deftest one-off-change-listener-test
  (let [log    (atom [])
        button (make :scene.control/button {:text "foo"})]
    (-> button .textProperty (.addListener (one-off-change-listener (fn [_ _ _] (swap! log conj "called")))))
    (is (= [] @log))

    (.setText button "bar")
    (is (= ["called"] @log))

    (.setText button "baz")
    (is (= ["called"] @log))

    (.setText button "foobar")
    (is (= ["called"] @log))

    (.setText button "barbar")
    (is (= ["called"] @log))

    (.setText button "bazbar")
    (is (= ["called"] @log))))
