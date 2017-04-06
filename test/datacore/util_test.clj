(ns datacore.util-test
  (:require [datacore.util :refer :all]
            [clojure.test :refer :all]
            [clojure.spec :as s]
            [clojure.spec.test :as stest]
            [clojure.spec.gen :as gen]))

(deftest take-exactly-test
  (is (= [1 2] (take-exactly 2 [1 2 3] :x)))
  (is (= [1 2 3] (take-exactly 3 [1 2 3] :x)))
  (is (= [1 2 3 :x :x :x] (take-exactly 6 [1 2 3] :x))))

(deftest diff-test
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
  (is (= [[:delete 0 0]
          [:insert 2 80]
          [:insert 2 70]
          [:delete 7 6]]
         (seq-diff-indices [0 1 2       3 4 5 6 7]
                           [  1 2 70 80 3 4 5 7])))

  (is (= [[:edit 2 2 10]]
         (seq-diff-indices [0 1 2  3 4 5 6 7]
                           [0 1 10 3 4 5 6 7])))

  (is (= [[:edit   2 {:f 9, :g 10} {:f 100, :g 10}]
          [:insert 4 80]
          [:insert 4 70]
          [:delete 9 6]]
         (seq-diff-indices [0 1 {:f 9   :g 10} 2       3 4 5 6]
                           [0 1 {:f 100 :g 10} 2 70 80 3 4 5])))

  (is (= [[:insert 0 3]
          [:insert 0 2]
          [:insert 0 1]]
         (seq-diff-indices [] [1 2 3])))

  (is (= [[:delete 0 1]
          [:delete 0 2]
          [:delete 0 3]]
         (seq-diff-indices [1 2 3] [])))

  (is (= [[:delete 0 []]
          [:delete 0 5]
          [:delete 0 {:m [[5 6 5 7 5] 9], :l 0, :b {}, :d 5}]]
         (seq-diff-indices [[] 5 {:m [[5 6 5 7 5] 9], :l 0, :b {}, :d 5}] []))))

(deftest tree-diff-test
  (is (= [[:delete [1] :b]]
         (tree-diff [:a :b :c :d :e]
                    [:a    :c :d :e])))

  (is (= [[:edit [:c] "foo" "bar"]
          [:edit [:b] 1 10]]
         (tree-diff {:b 1  :c "foo"}
                    {:b 10 :c "bar"})))

  (is (= [[:dissoc [:a] 6]
          [:assoc [:g] 900]
          [:assoc [:h] 100]
          [:edit [:b] 10 11]
          [:edit [:d] 40 41]]
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
    (is (= [[:assoc [:children 0 :root :confused?] false]
            [:assoc [:children 0 :root :focused?] true]
            [:delete [:children 1] {:type         :datacore.ui.view/window
                                    :window-style :transparent
                                    :root         {:type :datacore.ui.view/prompt
                                                   :id   "fec825ac-8419-4f32-b445-17d6e21cbbe9"}
                                    :id           "e931ae63-439c-48c4-a4fa-161fbd354de0"}]]
           (tree-diff tree-a tree-b))))

  (is (= [[:edit   [:a :c] "foo" "bar"]
          [:delete [:a :b 0] 0]
          [:insert [:a :b 2] 80]
          [:insert [:a :b 2] 70]
          [:delete [:a :b 7] 6]]
         (tree-diff {:a {:b [0 1 2       3 4 5 6]
                         :c "foo"}}
                    {:a {:b [  1 2 70 80 3 4 5]
                         :c "bar"}})))

  (is (= [[:edit   [2 :f] 9 100]
          [:insert [4]    80]
          [:insert [4]    70]
          [:delete [9]    6]]
         (tree-diff [0 1 {:f 9   :g 10} 2       3 4 5 6]
                    [0 1 {:f 100 :g 10} 2 70 80 3 4 5])))

  (is (= [[:dissoc [2 :g] 10]
          [:edit   [2 :f] 9 100]
          [:insert [4]    80]
          [:insert [4]    70]
          [:delete [9]    6]]
         (tree-diff [0 1 {:f 9   :g 10} 2       3 4 5 6]
                    [0 1 {:f 100}       2 70 80 3 4 5])))

  (let [tree-a {:a {:b [0 1 {:f 9   :g 10} 2       3 4 5 6]
                    :c "foo"}}
        tree-b {:a {:b [0 1 {:f 100 :g 10} 2 70 80 3 4 5]
                    :c "bar"}}]
    (is (= [[:edit   [:a :c] "foo" "bar"]
            [:edit   [:a :b 2 :f] 9 100]
            [:insert [:a :b 4] 80]
            [:insert [:a :b 4] 70]
            [:delete [:a :b 9] 6]]
           (tree-diff tree-a tree-b)))))

(deftest patch-test
  (is (= {:a {:b [0 1 {:f 100 :g 10} 2 70 80 3 4 5]
              :c "bar"}}
         (patch {:a {:b [0 1 {:f 9 :g 10} 2 3 4 5 6]
                     :c "foo"}}
                [[:edit   [:a :c] "foo" "bar"]
                 [:edit   [:a :b 2 :f] 9 100]
                 [:insert [:a :b 4] 80]
                 [:insert [:a :b 4] 70]
                 [:delete [:a :b 9] 6]])))
  (is (= {:foo "bar"}
         (patch 2 [[:edit [] 2 {:foo "bar"}]]))))

(def a [{:t [{:t 2, :v 7} 9 [2 8 8]]
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
         :b {:l [6 5 3], :k 1, :r 9, :c 6}}])

(def b [{:t {:y {:v 5, :y 8}, :l 8, :p {:s 8, :m 9}}
         :y [0]}
        5
        8
        {:k {:q [2 0], :v {:l 5, :r 5, :x 9}},
         :z {:p {:x 3, :k 7, :r 4, :m 9}, :w {:z 8}, :t {}, :u [2 7]}}])

(def dd [[:dissoc [0 :u] {:h 6}]
         [:assoc [0 :y] [0]]
         [:edit [0 :t] [{:t 2, :v 7} 9 [2 8 8]] {:y {:v 5, :y 8}, :l 8, :p {:s 8, :m 9}}]
         [:edit [0] 3 5]
         [:edit [0] [] 8]
         [:dissoc [0 :y] [{} [1 5 5 7 1] 3]]
         [:dissoc [0 :b] {:l [6 5 3], :k 1, :r 9, :c 6}]
         [:dissoc [0 :t] 6]
         [:dissoc [0 :x] {:r [], :u {:b 6, :k 4, :i 1, :a 2}, :h {:k 0, :a 2, :l 1}, :o {:a 3, :x 4, :u 9, :q 2}, :i 8}]
         [:assoc [0 :k] {:q [2 0], :v {:l 5, :r 5, :x 9}}]
         [:assoc [0 :z] {:p {:x 3, :k 7, :r 4, :m 9}, :w {:z 8}, :t {}, :u [2 7]}]])

(s/def ::limited-key
  (s/with-gen
    keyword?
    #(gen/elements [:a :b :c :d :e :f :g :h :i :j :k :l :m :n :o :p :q :r :s :t :u :v :w :x :y :z])))
(s/def ::limited-number #{0 1 2 3 4 5 6 7 8 9})
(s/def ::limited-map (s/map-of ::limited-key ::limited-value, :min-count 0 :max-count 5))
(s/def ::limited-vector (s/coll-of ::limited-value, :kind vector? :min-count 0 :max-count 5))
(s/def ::limited-value (s/or :number ::limited-number
                             :map    ::limited-map
                             :vector ::limited-vector))

(deftest diff-tree-test-check
  (stest/check
   [`tree-diff]
   {:gen {:datacore.util/tree-diff-input #(s/gen ::limited-value)}}))

(comment
  (first
   (stest/check
    [`tree-diff]
    {:gen {:datacore.util/tree-diff-input #(s/gen ::limited-value)}}))q
  )
