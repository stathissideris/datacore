(ns datacore.util-test
  (:require [datacore.util :refer :all]
            [clojure.test :refer :all]))

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
                           [0 1 {:f 100 :g 10} 2 70 80 3 4 5]))))

(deftest map-diff-test
  (is (= [[:delete [:a] 6]
          [:insert [:g] 900]
          [:insert [:h] 100]
          [:edit [:b] 10 11]
          [:edit [:d] 40 41]]
         (tree-diff {:a 6 :b 10 :c 90 :d 40 :e 100 :f 900}
                   {     :b 11 :c 90 :d 41 :e 100 :f 900 :g 900 :h 100}))))

(deftest tree-diff-test
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
    (is (= [[:insert [:children 0 :root :confused?] false]
            [:insert [:children 0 :root :focused?] true]
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

  (is (= [[:delete [2 :g] 10] ;;dissoc
          [:edit   [2 :f] 9 100]
          [:insert [4]    80]
          [:insert [4]    70]
          [:delete [9]    6]] ;;seq delete
         (tree-diff [0 1 {:f 9   :g 10} 2       3 4 5 6]
                    [0 1 {:f 100} 2 70 80 3 4 5])))

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
