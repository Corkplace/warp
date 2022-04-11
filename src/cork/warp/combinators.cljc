(ns cork.warp.combinators
  (:refer-clojure :exclude [+ * map cat])
  (:import #?(:clj [clojure.lang
                    PersistentVector
                    PersistentHashSet
                    PersistentTreeSet
                    Fn]))
  (:require
   #?(:clj  [clojure.core :as core]
      :cljs [cljs.core :as core])
   [cork.warp :as w]
   [cork.warp.macros :as m]
   [cork.warp.state :as s]
   [clojure.string :as str]))

(defn chain
  "Given a list of parsers, return a list of all the results."
  [parsers]
  (m/impl-parse state
    (loop [[parser & parsers] parsers
           state              state
           results            []]
      (if (nil? parser)
        (s/put-result state results)
        (let [state' (s/-parse parser state)]
          (if (s/error? state')
            (s/put-error state [:chain (:error state')])
            (recur parsers state' (conj results (:result state')))))))))

(defn alt
  "Given a list of parser, return the result of the first one that succeeds."
  [parsers]
  (m/impl-parse state
    (loop [parsers parsers
           errors  []]
      (if (empty? parsers)
        (s/put-error state [:alt errors])
        (let [next (s/-parse (first parsers) state)]
          (if (s/result? next)
            next
            (recur (rest parsers)
                   (conj errors (:error next)))))))))

(defn repeated
  "Match a parser from n to m times."
  [{:keys [from to] :or   {from 0 to 256}}
   parser]
  (m/impl-parse state
    (loop [results []
           next state]
      (if (= (count results) to)
        (s/put-result next results)
        (let [next (s/-parse parser next)]
          (if (s/error? next)
            (if (< (count results) from)
              (s/put-error state (cond-> {:parser  :repeated
                                          :from    from
                                          :to      to
                                          :matched (count results)}
                                   (s/error? next) (assoc :error (:error next))))
              (s/put-result next results))
            (recur (conj results (:result next)) next)))))))

(defn *
  "Match zero or more"
  [parser]
  (repeated {} parser))

(defn +
  "Match one or more"
  [parser]
  (repeated {:from 1} parser))

(defn lazy
  "Given a fn that returns a parser, uses the parser returned to parse the next state."
  [get-parser]
  (m/impl-parse state
    (let [parser (get-parser)]
      (s/-parse parser state))))

(defn then
  "Apply f onto the result of the given parser."
  [get-parser parser]
  (m/impl-parse state
    (let [next (s/-parse parser state)]
      (if (s/error? next)
        next
        (let [parser (get-parser next)
              next   (s/-parse parser next)]
          (if (s/error? next)
            (s/put-error state {:parser :then
                                :error  (:error next)})
            next))))))

(defn map
  "Apply f onto the result of the given parser."
  [f parser]
  (m/impl-parse state
    (let [state' (s/-parse parser state)]
      (if (s/error? state')
        state'
        (update state' :result f state state')))))

(def first' (fn [& args] (ffirst args)))
(def second' (fn [& args] ((comp second first) args)))

(def maybe
  (comp
   (partial map first')
   (partial repeated {:from 0 :to 1})))

(defn sep-by
  [sep-parser parser]
  (map (fn [[first rest] _ _]
         (concat [first] rest))
       (chain [parser (* (map second' (chain [sep-parser parser])))])))

(defn wrapped
  [left right parser]
  (map second' (chain [left parser right])))

#?(:clj  (extend-protocol s/Parser
           String
           (-parse [this state]
             (let [parser (chain (into [] this))
                   parser (map (fn [result _ _]
                                 (str/join "" result))
                               parser)]
               (s/-parse parser state)))
           Fn
           (-parse [this state]
             (s/-parse (lazy this) state))
           PersistentVector
           (-parse [this state]
             (s/-parse (chain this) state))
           PersistentHashSet
           (-parse [this state]
             (s/-parse (alt this) state))
           PersistentTreeSet
           (-parse [this state]
             (s/-parse (alt this) state)))
   :cljs (extend-protocol s/Parser
           core/PersistentVector
           (-parse [this state]
             (-parse (apply chain this) state))
           core/PersistentHashSet
           (-parse [this state]
             (-parse (apply alt this) state))
           function
           (-parse [this state]
             (-parse (lazy this) state))
           string
           (-parse [this state]
             (-parse (literal this) state))))
