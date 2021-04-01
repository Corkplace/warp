(ns cork.warp.util
  (:require [cork.warp :as w]
            [cork.warp.text :refer [letter]]
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

(defn tagged
  [parser tag]
  (-> [tag parser]
      (w/map
       (fn [[_ result] _ _]
         result))))
