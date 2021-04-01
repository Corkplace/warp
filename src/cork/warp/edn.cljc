(ns cork.warp.edn
  (:refer-clojure :exclude [symbol keyword expr list vector set quote])
  (:require
   [clojure.pprint :refer [pprint]]
   [clojure.string :as string]
   [cork.warp :as w :refer [maybe]]
   [cork.warp.util :as u :refer [sep-by token blank node wrapped-by]]
   [cork.warp.text :as t]))

(defn children [parser & names]
  (w/map parser
         (fn [node _ _]
           (assoc node :children (into [] names)))))

(def symbol
  (let [special       (t/char-of "<>*+-_")
        simple-symbol (-> (w/+ #{special t/letter t/digit})
                          (w/map
                           (fn [result _ _]
                             (string/join "" result))))
        dot-symbol    (-> simple-symbol
                          (u/sep-by \.)
                          (w/map
                           (fn [names _ _]
                             {:name (string/join "." names)})))
        ns-symbol     (-> [dot-symbol \/ dot-symbol]
                          (w/map
                           (fn [[namespace _ name] _ _]
                             {:namespace (:name namespace)
                              :name      (:name name)})))]
    (-> (w/alt ns-symbol dot-symbol)
        (u/node :symbol)
        (w/map
         (fn [result _ _]
           (-> result
               (merge (:value result))
               (dissoc :value)
               (assoc :children [:name :namespace])))))))

(def keyword
  (let [special        (t/char-of "<>*+-_")
        simple-keyword (-> (w/+ #{special t/letter t/digit})
                           (w/map
                            (fn [result _ _]
                              (string/join "" result))))
        dot-keyword    (-> simple-keyword
                           (u/sep-by \.)
                           (w/map
                            (fn [names _ _]
                              {:name (string/join "." names)})))
        ns-keyword     (-> [dot-keyword \/ dot-keyword]
                           (w/map
                            (fn [[namespace _ name] _ _]
                              {:namespace (:name namespace)
                               :name      (:name name)})))]
    (-> [(w/alt "::" ":") (w/alt ns-keyword dot-keyword)]
        (u/node :keyword)
        (w/map
         (fn [result _ _]
           (let [qual? (= "::" (first (:value result)))]
             (cond-> result
               :do   (merge (second (:value result)))
               :do   (dissoc :value)
               :do   (assoc :children [:name :namespace])
               qual? (assoc :qualified? true)
               qual? (update :children conj :qualified?))))))))

(declare expr)

(defn list
  []
  (-> expr
      (u/sep-by (u/token u/blank))
      (u/wrapped-by (u/token "(") (u/token ")"))
      (u/node :list)
      (children :value)))

(defn vector
  []
  (-> expr
      (sep-by (token blank))
      (wrapped-by (token "[") (token "]"))
      (node :vector)
      (children :value)))

(defn set
  []
  (-> expr
      (sep-by (token blank))
      (wrapped-by (token "#{") (token "}"))
      (node :set)
      (children :value)))

(defn quote
  []
  (-> expr
      (u/tagged "'")
      (node :quote)
      (children :value)))

(defn expr []
  (w/alt keyword symbol list vector set quote))

(defn program []
  (-> expr
      (sep-by (token blank))
      (maybe)
      (node :program)
      (children :value)))
