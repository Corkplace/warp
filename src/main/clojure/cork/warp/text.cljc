(ns cork.warp.text
  (:require [cork.warp.combinators :as c]
            [cork.warp :as w]
            [clojure.string :as string]))

(defn char-of
  [text]
  {:pre [(string? text)]}
  (c/alt (map w/match text)))

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

(defn- uppercase-character [c]
  #?(:clj (Character/toUpperCase c)
     :cljs (.toUpperCase c)))

(defn- lowercase-character [c]
  #?(:clj (Character/toLowerCase c)
     :cljs (.toLowerCase c)))

(defn insensitive
  [text]
  (->> (seq text)
       (mapv (fn [letter]
               (let [l (lowercase-character letter)
                     u (uppercase-character letter)]
                 (if (= l u)
                   letter
                   #{l u}))))
       (compact)))

(def word (comp compact c/+ (partial c/alt [letter \- digit])))

(def punctuation (char-of "!?,.;:"))
