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
  (c/alt [\newline \tab \space]))

(defn insensitive [text]
  (-> (->> (string/split text #"")
           (filter boolean)
           (mapv (fn [letter]
                   #{(string/lower-case letter)
                     (string/upper-case letter)})))
      (c/map (fn [result _ _]
               (string/join "" result)))))

(def compact
  (partial c/map
           (fn [result _ _]
             (string/join "" result))))

(def word (comp compact
                c/+
                (partial c/alt [letter \- digit])))

(def punctuation (char-of "!?,.;:"))
