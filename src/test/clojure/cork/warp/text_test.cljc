(ns cork.warp.text-test
  (:require [cork.warp :as sut]
            [cork.warp.text :as text]
            #?(:clj [clojure.test :as t :refer [deftest is]]
               :cljs [cljs.test :as t :refer [deftest is]])))

(deftest test-digit
  (let [{:keys [result]} (sut/info text/digit "123")]
    (is (= result \1))))

(deftest test-letter
  (let [{:keys [result]} (sut/info text/letter "abc")]
    (is (= result \a))))
