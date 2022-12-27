(ns cork.warp-test
  (:require [cork.warp :as sut]
            #?(:clj [clojure.test :as t :refer [deftest is run-tests]]
               :cljs [cljs.test :as t :refer [deftest is run-tests]])))

(deftest test-one
  (let [{:keys [offset source result]} (sut/info sut/one "abc")]
    (is (= offset 1))
    (is (= result \a))
    (is (= source (seq "bc")))))

(deftest test-match
  (let [{:keys [offset source result]} (sut/info (sut/match \a) "abc")]
    (is (= offset 1))
    (is (= result \a))
    (is (= source (seq "bc")))))

(deftest test-eof
  (let [{:keys [result]} (sut/info sut/eof "")]
    (is (= result :eof))))

;; (deftest test-text-macher
;;   (is (= "match-me" (sut/parse "match-me" "match-me")))
;;   (is (nil? (sut/parse  "do i match?" "no")))
;;   (let [{:keys [offset]}
;;         (sut/info "012" "012")]
;;     (is (= offset 3))))

;; (deftest test-sequence-matcher
;;   (is (= ["a" "b"] (sut/parse ["a" "b"] "ab")))
;;   (is (nil? (sut/parse ["a" "b" "c"] "ab"))))

;; (deftest test-alt-matcher
;;   (let [parser [#{"A" "a"} "b" "c"]]
;;     (is (= ["A" "b" "c"] (sut/parse parser "Abc")))
;;     (is (= ["a" "b" "c"] (sut/parse parser "abc")))))

(defn -main [& _args]
  (run-tests 'cork.warp-test))
