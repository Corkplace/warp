(ns cork.warp.util
  (:require [cork.warp :as w]
            [clojure.pprint :refer [pprint]]
            [clojure.set :as set]))

(def blank #{" " "\n" "\t" "\r\n"})

(defn token [parser]
  (w/map [parser (w/* blank)]
         (fn [result pre post]
           (first result))))

(defn node [parser op]
  (w/map parser
         (fn [result pre post]
           {:op       op
            :value    result
            :location {:start (:offset pre)
                       :end   (:offset post)}})))

(defn join-nodes [parser]
  (w/map parser
         (fn [results _ _]
           (reduce
            (fn [m {:keys [op value]}]
              (assoc m op value))
            {}
            results))))

(defn ssecond [v] (second (second v)))

(defn list-of
  [sep parser]
  (w/map [parser (w/maybe [sep #(list-of sep parser)])]
         (fn [result pre post]
           (if (nil? (second result))
             (list (first result))
             (conj (ssecond result) (first result))))))

(defn sep-by
  [parser sep]
  (w/map [parser (w/maybe [sep #(sep-by parser sep)])]
         (fn [result pre post]
           (if (nil? (second result))
             (list (first result))
             (conj (ssecond result) (first result))))))

(defn wrapped-by
  [parser first last]
  (w/map [first parser last]
         (fn [[first parser last] _ _]
           parser)))

(defn finally
  [parser tag]
  (w/map [parser tag]
         (fn [result _ _]
           (first result))))

(defn enhance
  [f & args]
  #(apply f % args))
