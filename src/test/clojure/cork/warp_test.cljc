(ns cork.warp-test
  (:require [cork.warp :as sut]
            [cork.warp.combinators :as c]
            [cork.warp.state :as state]
            [clojure.string :as string]
            #?(:clj [clojure.test :as t :refer [deftest is run-tests]]
               :cljs [cljs.test :as t :refer [deftest is run-tests]])))

(deftest test-text-macher
  (is (= "match-me" (sut/parse "match-me" "match-me")))
  (is (nil? (sut/parse  "do i match?" "no")))
  (let [{:keys [offset]}
        (sut/info "012" "012")]
    (is (= offset 3))))

(deftest test-sequence-matcher
  (is (= ["a" "b"] (sut/parse ["a" "b"] "ab")))
  (is (nil? (sut/parse ["a" "b" "c"] "ab"))))

(deftest test-alt-matcher
  (let [parser [#{"A" "a"} "b" "c"]]
    (is (= ["A" "b" "c"] (sut/parse parser "Abc")))
    (is (= ["a" "b" "c"] (sut/parse parser "abc")))))

(deftest test-repeated-matcher
  (let [parser (c/repeated {} "a")]
    (is (= [] (sut/parse parser "bbb")))
    (is (= ["a"] (sut/parse parser "abbb")))
    (is (= ["a" "a"] (sut/parse parser "aabbb"))))
  (let [parser (c/repeated {:from 3} "a")]
    (is (= ["a" "a" "a"] (sut/parse parser "aaabbb")))
    (is (nil? (sut/parse parser "abbb"))))
  (let [parser (c/repeated {:to 2} "a")]
    (is (= ["a"] (sut/parse parser "abbb")))
    (is (= ["a" "a"] (sut/parse parser "aabbb")))
    (let [state (sut/info parser "aaaaaabbb")]
      (is (= ["a" "a"] (:result state)))
      (is (= \a (state/peek state)))
      (is (= 2 (:offset state))))))

(deftest test-map-matcher
  (let [parser (c/map (fn [name _ _] (string/upper-case name)) "name")
        state  (sut/info parser "name")]
    (is (= "NAME" (:result state)))
    (is (= 4 (:offset state))))
  (let [parser (c/map (fn [_ {start :offset} {end :offset}] [start end]) "name")
        state  (sut/info parser "name")]
    (is (= [0 4] (:result state)))))

(defn -main [& _args]
  (run-tests 'cork.warp-test))
