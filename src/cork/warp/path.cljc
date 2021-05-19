(ns cork.warp.path
  (:require [cork.warp :as w]
            [cork.warp.util :as u]
            [cork.warp.text :as t]
            [clojure.set :as set]
            [clojure.string :as string]))

(def ^:dynamic os-delim "/")
(def ^:dynamic os-prefix "/")

(def ^:private sub-delims #{"!"  "$"  "&"  "'"  "("  ")"
                            "*"  "+"  ","  ";"  "="})

(def ^:private unreserved #{t/letter t/digit \- \. \_ "~"})

(def ^:private pchar (w/alt unreserved sub-delims ":" "@"))

(def segment (t/join (w/+ pchar)))

(defn when-nil [parser value]
  (w/map parser
         (fn [result _ _]
           (or result value))))

(defn path []
  (-> segment
      (u/sep-by os-delim)
      (w/maybe)
      (u/tagged os-prefix)
      (u/finally (w/maybe os-delim))
      (u/token)
      (when-nil '())
      (u/node :path)
      ))
