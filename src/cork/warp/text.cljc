(ns cork.warp.text
  (:require [cork.warp :as w]
            [clojure.set :as set]))

(defn char-of
  [text]
  (into #{} text))

(def digit (char-of "0123456789"))

(def lower (char-of "abcdefghijklmnopqrstuvwxyz"))
(def upper (char-of "ABCDEFGHIJKLMNOPQRSTUVWXYZ"))
(def letter (set/union lower upper))
