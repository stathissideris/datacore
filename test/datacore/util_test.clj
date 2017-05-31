(ns datacore.util-test
  (:refer-clojure :exclude [meta alter-meta! future])
  (:require [datacore.util :refer :all]
            [clojure.test :refer :all]
            [clojure.spec :as s]
            [clojure.spec.test :as stest]
            [clojure.spec.gen :as gen]
            ;;[clojure.test.check :as tc]
            ))

(deftest take-exactly-test
  (is (= [1 2] (take-exactly 2 [1 2 3] :x)))
  (is (= [1 2 3] (take-exactly 3 [1 2 3] :x)))
  (is (= [1 2 3 :x :x :x] (take-exactly 6 [1 2 3] :x))))

(deftest seq-diff-test
  (is (= [[:delete :a]]
         (seq-diff [:a] [])))
  (is (= [[:delete nil]]
         (seq-diff [nil] [])))
  (is (= [[:delete 0]
          [:same 1]
          [:same 2]
          [:same 3]
          [:insert :a]
          [:insert :b]
          [:insert :c]
          [:same 4]
          [:same 5]
          [:same 6]
          [:delete 7]
          [:delete 8]]
         (seq-diff [0 1 2 3 4 5 6 7 8] [1 2 3 :a :b :c 4 5 6])))
  (is (= [[:delete 0]
          [:same 1]
          [:same 2]
          [:same 3]
          [:edit :e :a]
          [:edit :f :b]
          [:edit :g :c]
          [:same 4]
          [:same 5]
          [:same 6]
          [:delete 7]
          [:delete 8]]
         (seq-diff [0 1 2 3 :e :f :g 4 5 6 7 8] [1 2 3 :a :b :c 4 5 6])))
  (is (= [[:delete 0]
          [:same 1]
          [:same 2]
          [:same 3]
          [:edit :e :a]
          [:edit :f :b]
          [:edit :g :c]
          [:same 4]
          [:same 5]
          [:insert 10]
          [:same 6]
          [:delete 7]
          [:delete 8]]
         (seq-diff [0 1 2 3 :e :f :g 4 5 6 7 8] [1 2 3 :a :b :c 4 5 10 6]))))

(deftest seq-diff-indices-test
  (is (= [{:type :delete :index 0 :value 0}
          {:type :insert :index 2 :value 80}
          {:type :insert :index 2 :value 70}
          {:type :delete :index 7 :value 6}]
         (seq-diff-indices [0 1 2       3 4 5 6 7]
                           [  1 2 70 80 3 4 5 7])))

  (is (= [{:type :edit :index 2 :old 2 :value 10}]
         (seq-diff-indices [0 1 2  3 4 5 6 7]
                           [0 1 10 3 4 5 6 7])))

  (is (= [{:type :edit :index 2 :old {:f 9, :g 10} :value {:f 100, :g 10}}
          {:type :insert :index 4 :value 80}
          {:type :insert :index 4 :value 70}
          {:type :delete :index 9 :value 6}]
         (seq-diff-indices [0 1 {:f 9   :g 10} 2       3 4 5 6]
                           [0 1 {:f 100 :g 10} 2 70 80 3 4 5])))

  (is (= [{:type :insert :index 0 :value 3}
          {:type :insert :index 0 :value 2}
          {:type :insert :index 0 :value 1}]
         (seq-diff-indices [] [1 2 3])))

  (is (= [{:type :delete :index 0 :value 1}
          {:type :delete :index 0 :value 2}
          {:type :delete :index 0 :value 3}]
         (seq-diff-indices [1 2 3] [])))

  (is (= [{:type :delete :index 0 :value []}
          {:type :delete :index 0 :value 5}
          {:type :delete :index 0 :value {:m [[5 6 5 7 5] 9], :l 0, :b {}, :d 5}}]
         (seq-diff-indices [[] 5 {:m [[5 6 5 7 5] 9], :l 0, :b {}, :d 5}] []))))

(deftest tree-diff-test
  (is (= [{:type :insert :path [1] :value 1 :struct :vector}]
         (tree-diff [0] [0 1])))

  (is (= [{:type :delete :path [1] :value :b :struct :vector}]
         (tree-diff [:a :b :c :d :e]
                    [:a    :c :d :e])))

  (is (= [{:type :edit :path [:c] :old "foo" :value "bar" :struct :map}
          {:type :edit :path [:b] :old 1 :value 10 :struct :map}]
         (tree-diff {:b 1  :c "foo"}
                    {:b 10 :c "bar"})))

  (is (= [{:type :dissoc :path [:a] :value 6 :struct :map}
          {:type :assoc :path [:g] :value 900 :struct :map}
          {:type :assoc :path [:h] :value 100 :struct :map}
          {:type :edit :path [:b] :old 10 :value 11 :struct :map}
          {:type :edit :path [:d] :old 40 :value 41 :struct :map}]
         (tree-diff {:a 6 :b 10 :c 90 :d 40 :e 100 :f 900}
                    {     :b 11 :c 90 :d 41 :e 100 :f 900 :g 900 :h 100})))

  (let [tree-a {:type :datacore.ui.view/top-level
                :children
                [{:type :datacore.ui.view/window
                  :title "datacore"
                  :dimensions [1000 800]
                  :root
                  {:type :datacore.ui.view/cell
                   :id "87d3ba15-16f6-434d-8707-e34f34cbc9cf"}
                  :id "a4b55b75-3156-444d-af49-5b8657886859"}
                 {:type :datacore.ui.view/window
                  :window-style :transparent
                  :root {:type :datacore.ui.view/prompt :id "fec825ac-8419-4f32-b445-17d6e21cbbe9"}
                  :id "e931ae63-439c-48c4-a4fa-161fbd354de0"}]
                :id "39d4c029-0df5-4daa-8e65-1bad38474504"}
        tree-b {:type :datacore.ui.view/top-level
                :children
                [{:type :datacore.ui.view/window
                  :title "datacore"
                  :dimensions [1000 800]
                  :root
                  {:type :datacore.ui.view/cell
                   :focused? true
                   :confused? false
                   :id "87d3ba15-16f6-434d-8707-e34f34cbc9cf"}
                  :id "a4b55b75-3156-444d-af49-5b8657886859"}]
                :id "39d4c029-0df5-4daa-8e65-1bad38474504"}]
    (is (= [{:type :assoc :path [:children 0 :root :confused?] :value false :struct :map}
            {:type :assoc :path [:children 0 :root :focused?] :value true :struct :map}
            {:type :delete :path [:children 1] :value {:type         :datacore.ui.view/window
                                                       :window-style :transparent
                                                       :root         {:type :datacore.ui.view/prompt
                                                                      :id   "fec825ac-8419-4f32-b445-17d6e21cbbe9"}
                                                       :id           "e931ae63-439c-48c4-a4fa-161fbd354de0"}
             :struct :vector}]
           (tree-diff tree-a tree-b))))

  (is (= [{:type :edit   :path [:a :c] :old "foo" :value "bar" :struct :map}
          {:type :delete :path [:a :b 0] :value 0 :struct :vector}
          {:type :insert :path [:a :b 2] :value 80 :struct :vector}
          {:type :insert :path [:a :b 2] :value 70 :struct :vector}
          {:type :delete :path [:a :b 7] :value 6 :struct :vector}]
         (tree-diff {:a {:b [0 1 2       3 4 5 6]
                         :c "foo"}}
                    {:a {:b [  1 2 70 80 3 4 5]
                         :c "bar"}})))

  (is (= [{:type :edit   :path [2 :f] :old 9 :value 100 :struct :map}
          {:type :insert :path [4] :value 80 :struct :vector}
          {:type :insert :path [4] :value 70 :struct :vector}
          {:type :delete :path [9] :value 6 :struct :vector}]
         (tree-diff [0 1 {:f 9   :g 10} 2       3 4 5 6]
                    [0 1 {:f 100 :g 10} 2 70 80 3 4 5])))

  (is (= [{:type :dissoc :path [2 :g] :value 10 :struct :map}
          {:type :edit   :path [2 :f] :old 9 :value 100 :struct :map}
          {:type :insert :path [4] :value 80 :struct :vector}
          {:type :insert :path [4] :value 70 :struct :vector}
          {:type :delete :path [9] :value 6 :struct :vector}]
         (tree-diff [0 1 {:f 9   :g 10} 2       3 4 5 6]
                    [0 1 {:f 100}       2 70 80 3 4 5])))

  (let [tree-a {:a {:b [0 1 {:f 9   :g 10} 2       3 4 5 6]
                    :c "foo"}}
        tree-b {:a {:b [0 1 {:f 100 :g 10} 2 70 80 3 4 5]
                    :c "bar"}}]
    (is (= [{:type :edit   :path [:a :c] :old "foo" :value "bar" :struct :map}
            {:type :edit   :path [:a :b 2 :f] :old 9 :value 100 :struct :map}
            {:type :insert :path [:a :b 4] :value 80 :struct :vector}
            {:type :insert :path [:a :b 4] :value 70 :struct :vector}
            {:type :delete :path [:a :b 9] :value 6 :struct :vector}]
           (tree-diff tree-a tree-b))))

  (let [a    [{:t [{:t 2, :v 7} 9 [2 8 8]]
               :u {:h 6}}
              3
              []
              {:x
               {:r [],
                :u {:b 6, :k 4, :i 1, :a 2},
                :h {:k 0, :a 2, :l 1},
                :o {:a 3, :x 4, :u 9, :q 2},
                :i 8},
               :t 6,
               :y [{} [1 5 5 7 1] 3],
               :b {:l [6 5 3], :k 1, :r 9, :c 6}}]

        b    [{:t {:y {:v 5, :y 8}, :l 8, :p {:s 8, :m 9}}
               :y [0]}
              5
              8
              {:k {:q [2 0], :v {:l 5, :r 5, :x 9}},
               :z {:p {:x 3, :k 7, :r 4, :m 9}, :w {:z 8}, :t {}, :u [2 7]}}]
        diff (tree-diff a b)]
    (is (= b (patch a diff)))))

(deftest patch-test
  (is (= {:a {:b [0 1 {:f 100 :g 10} 2 70 80 3 4 5]
              :c "bar"}}
         (patch {:a {:b [0 1 {:f 9 :g 10} 2 3 4 5 6]
                     :c "foo"}}
                [{:type :edit   :path [:a :c] :old "foo" :value "bar"}
                 {:type :edit   :path [:a :b 2 :f] :old 9 :value 100}
                 {:type :insert :path [:a :b 4] :value 80}
                 {:type :insert :path [:a :b 4] :value 70}
                 {:type :delete :path [:a :b 9] :value 6}])))
  (is (= {:foo "bar"}
         (patch 2 [{:type :edit :path [] :old 2 :value {:foo "bar"}}])))
  (is (= {:a "foo"}
         (patch nil [{:type :edit :path [] :old nil :value {:a "foo"}}]))))

(s/def ::limited-key
  (s/with-gen
    keyword?
    #(gen/elements [:a :b :c :d :e :f :g :h :i :j ;;:k :l :m :n :o :p :q :r :s :t :u :v :w :x :y :z
                    ])))
(s/def ::limited-number #{0 1 2 3 4 5 6 7 8 9})
(s/def ::limited-map (s/map-of (s/nilable ::limited-key)
                               (s/nilable ::limited-value) :min-count 0 :max-count 10))
(s/def ::limited-vector (s/coll-of (s/nilable ::limited-value) :kind vector? :min-count 0 :max-count 10))
(s/def ::limited-value (s/nilable
                        (s/or :number ::limited-number
                              :map    ::limited-map
                              :vector ::limited-vector)))

(comment
  (stest/summarize-results
   (binding [clojure.spec/*recursion-limit* 3]
     (stest/check
      [`tree-diff]
      {:gen                          {:datacore.util/tree-diff-input #(s/gen ::limited-value)}
       :clojure.spec.test.check/opts {:num-tests 75}}))))
