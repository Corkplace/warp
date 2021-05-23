(ns cork.warp.parsers.mj-test
  (:require [cork.warp.parsers.mj :as sut]
            [cork.warp :as w]
            [clojure.string :as string]
            #?(:clj [clojure.test :as t :refer [deftest is run-tests]]
               :cljs [cljs.test :as t :include-macros true])))

(deftest test-paragraphs
  (is (= (w/parse sut/parser "name")
         {:op :page
          :content [{:op :paragraph
                     :content [{:op :text
                                :content "name"}]}]})))
