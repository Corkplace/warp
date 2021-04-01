(ns cork.warp-test
  (:require [cork.warp :as sut]
            [clojure.string :as string]
            #?(:clj [clojure.test :as t :refer [deftest is]]
               :cljs [cljs.test :as t :include-macros true])))

(deftest test-text-macher
  (is (= "match-me" (sut/parse "match-me" "match-me")))
  (is (nil? (sut/parse  "do i match?" "no")))
  (let [{:keys [offset]}
        (sut/-parse "012" (sut/make-state "012"))]
    (is (= offset 3))))

(deftest test-sequence-matcher
  (is (= ["a" "b"] (sut/parse ["a" "b"] "ab")))
  (is (nil? (sut/parse ["a" "b" "c"] "ab"))))

(deftest test-alt-matcher
  (let [parser [#{"A" "a"} "b" "c"]]
    (is (= ["A" "b" "c"] (sut/parse parser "Abc")))
    (is (= ["a" "b" "c"] (sut/parse parser "abc")))))

(deftest test-repeated-matcher
  (let [parser (sut/repeated "a")]
    (is (= [] (sut/parse parser "bbb")))
    (is (= ["a"] (sut/parse parser "abbb")))
    (is (= ["a" "a"] (sut/parse parser "aabbb"))))
  (let [parser (sut/repeated "a" :from 3)]
    (is (= ["a" "a" "a"] (sut/parse parser "aaabbb")))
    (is (nil? (sut/parse parser "abbb"))))
  (let [parser (sut/repeated "a" :to 2)]
    (is (= ["a"] (sut/parse parser "abbb")))
    (is (= ["a" "a"] (sut/parse parser "aabbb")))
    (let [state (sut/-parse parser (sut/make-state "aaaaaabbb"))]
      (is (= ["a" "a"] (:result state)))
      (is (= "a" (sut/slice state)))
      (is (= 2 (:offset state))))))

(deftest test-map-matcher
  (let [parser (sut/map
                "name"
                (fn [name _ _]
                  (string/upper-case name)))
        state (sut/info parser "name")]
    (is (= "NAME" (:result state)))
    (is (= 4 (:offset state))))
  (let [parser (sut/map
                "name"
                (fn [_ {start :offset} {end :offset}]
                  [start end]))
        state (sut/info parser "name")]
    (is (= [0 4] (:result state)))))
