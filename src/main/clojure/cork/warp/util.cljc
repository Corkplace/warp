(ns cork.warp.util
  (:require [cork.warp :as w]
            [cork.warp.combinators :as c]
            [clojure.pprint :refer [pprint]]
            [clojure.set :as set]))

(def blank #{\space \newline \tab [\return \newline]})

(defn token [parser]
  (c/map
   (fn [result pre post]
     (first result))
   [parser (c/* blank)]))

(defn node [parser op]
  (c/map
   (fn [result pre post]
     {:op       op
      :value    result
      :location {:start (:offset pre)
                 :end   (:offset post)}})
   parser))

(defn join-nodes [parser]
  (c/map
   (fn [results _ _]
     (reduce
      (fn [m {:keys [op value]}]
        (assoc m op value))
      {}
      results))
   parser))

(defn ssecond [v] (second (second v)))

(defn list-of
  [sep parser]
  (c/map
   (fn [result pre post]
     (if (nil? (second result))
       (list (first result))
       (conj (ssecond result) (first result))))
   [parser (c/maybe [sep #(list-of sep parser)])]))

(defn sep-by
  [parser sep]
  (c/map (fn [result pre post]
           (if (nil? (second result))
             (list (first result))
             (conj (ssecond result) (first result))))
         [parser (c/maybe [sep #(sep-by parser sep)])]))

(defn wrapped-by
  [parser first last]
  (c/map
   (fn [[first parser last] _ _]
     parser)
   [first parser last]))

(defn finally
  [parser tag]
  (c/map
   (fn [result _ _]
     (first result))
   [parser tag]))

(defn enhance
  [f & args]
  #(apply f % args))
