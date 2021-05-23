(ns cork.warp.combinators
  (:refer-clojure :exclude [+ * map])
  (:import #?(:clj [clojure.lang
                    PersistentVector
                    PersistentHashSet
                    PersistentTreeSet
                    Fn]))
  (:require
   [cork.warp :refer [impl-parse -parse Parser]]
   [cork.warp.state :refer [increment-offset put-error put-result slice error? result?]]))

(def one
  "Matches any one character slice."
  (impl-parse state
    (let [value (slice state)]
      (if (nil? value)
        (put-error state {:parser :match-one
                          :given  :eof})
        (increment-offset (put-result state value))))))

(defn chain
  "Given a list of parsers, return a list of all the results."
  [& parsers]
  (impl-parse state
    (loop [results []
           parsers parsers
           next    state]
      (if (empty? parsers)
        (put-result next results)
        (let [parser (first parsers)
              next   (-parse parser next)]
          (if (error? next)
            (put-error state {:parser      :chain
                              :failed-with (:error next)})
            (recur (conj results (:result next)) (rest parsers) next)))))))

(defn alt
  "Given a list of parser, return the result of the first one that succeeds."
  [& parsers]
  (impl-parse state
    (loop [parsers parsers
           errors  []]
      (if (empty? parsers)
        (put-error state {:parser :alt
                          :errors errors})
        (let [next (-parse (first parsers) state)]
          (if (result? next)
            next
            (recur (rest parsers)
                   (conj errors (:error next)))))))))

(defn repeated
  "Match a parser from n to m times."
  [parser & {:keys [from to]
             :or   {from 0 to 256}}]
  (impl-parse state
    (loop [results []
           next    state]
      (if (= (count results) to)
        (put-result next results)
        (let [next (-parse parser next)]
          (if (error? next)
            (if (< (count results) from)
              (put-error state (cond-> {:parser :repeated
                                        :from from
                                        :to to
                                        :matched (count results)}
                                 (error? next) (assoc :error (:error next))))
              (put-result next results))
            (recur (conj results (:result next)) next)))))))

(defn *
  "Match zero or more"
  [parser]
  (repeated parser))

(defn +
  "Match one or more"
  [parser]
  (repeated parser :from 1))

(defn lazy
  "Given a fn that returns a parser, uses the parser returned to parse the next state."
  [get-parser]
  (impl-parse state
    (let [parser (get-parser)]
      (-parse parser state))))

(defn then
  "Apply f onto the result of the given parser."
  [parser get-parser]
  (impl-parse state
    (let [next (-parse parser state)]
      (if (error? next)
        next
        (let [parser (get-parser next)
              next   (-parse parser next)]
          (if (error? next)
            (put-error state {:parser :then
                              :error  (:error next)})
            next))))))

(defn map
  "Apply f onto the result of the given parser."
  [parser f]
  (impl-parse state
    (let [next (-parse parser state)]
      (if (error? next)
        next
        (update next :result f state next)))))

(defn maybe
  [parser]
  (map (repeated parser :from 0 :to 1)
       (fn [result _ _]
         (first result))))

#?(:clj (extend-protocol Parser
          Fn
          (-parse [this state]
            (-parse (lazy this) state))
          PersistentVector
          (-parse [this state]
            (-parse (apply chain this) state))
          PersistentHashSet
          (-parse [this state]
            (-parse (apply alt this) state))
          PersistentTreeSet
          (-parse [this state]
            (-parse (apply alt this) state)))
   :cljs (extend-protocol Parser
           string
           (-parse [this state]
             (-parse (text this) state))))
