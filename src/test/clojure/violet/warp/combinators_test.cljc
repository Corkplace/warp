(ns violet.warp.combinators-test
  (:require [violet.warp :as sut]
            [violet.warp.combinators :as c]
            [violet.warp.state :as state]
            [clojure.string :as string]
            #?(:clj [clojure.test :as t :refer [deftest is run-tests]]
               :cljs [cljs.test :as t :refer [deftest is run-tests]])))

(deftest test-character
  (let [{:keys [result]} (sut/info \a "abc")]
    (is (= result \a))))

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
