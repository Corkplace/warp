(ns cork.warp.text
  (:require [cork.warp :as w]
            [cork.warp.util :as u]
            [clojure.set :as set]
            [clojure.string :as string]))

(defn char-of
  [text]
  (into #{} text))

(def digit (char-of "0123456789"))

(def lower (char-of "abcdefghijklmnopqrstuvwxyz"))

(def upper (char-of "ABCDEFGHIJKLMNOPQRSTUVWXYZ"))

(def letter (set/union lower upper))

(defn insensitive [text]
  (-> (->> (string/split text #"")
           (filter boolean)
           (mapv (fn [letter]
                   #{(string/lower-case letter)
                     (string/upper-case letter)})))
      (w/map (fn [result _ _]
               (string/join "" result)))))


(comment
 (def method
   (as-> ["get" "put" "post" "options" "delete"] $
     (map insensitive $)
     (into #{} $)
     (u/node $ :method)
     (u/token $))))

(defn join
  ([parser] (join parser ""))
  ([parser with]
   (w/map parser (fn [r _ _]
                   (string/join with r)))))

(def word (join (w/+ #{letter \- digit})))

(def punctuation (char-of "!?,.;:"))
