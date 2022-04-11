(ns cork.warp.text
  (:require [cork.warp :as w]
            [cork.warp.util :as u]
            [cork.warp.combinators :as c]
            [clojure.set :as set]
            [clojure.string :as string]))

(defn char-of
  [text]
  (c/alt (into [] (seq text))))

(def digit (char-of "0123456789"))

(def lower (char-of "abcdefghijklmnopqrstuvwxyz"))

(def upper (char-of "ABCDEFGHIJKLMNOPQRSTUVWXYZ"))

(def letter (c/alt [lower upper]))

(def whitespace
  (c/alt [\newline \tab \space \return]))

(def compact
  (partial c/map
           (fn [result _ _]
             (string/join "" result))))

(defn insensitive [text]
  (->> (seq text)
       (mapv (fn [letter]
               (let [l (Character/toLowerCase letter)
                     u (Character/toUpperCase letter)]
                 (if (= l u)
                   letter
                   #{l u}))))
       (compact)))

(def word (comp compact c/+ (partial c/alt [letter \- digit])))

(def punctuation (char-of "!?,.;:"))
