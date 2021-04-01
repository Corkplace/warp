(ns cork.warp
  (:refer-clojure :exclude [+ * map])
  (:import #?(:clj [clojure.lang
                    PersistentVector
                    PersistentHashSet
                    PersistentTreeSet
                    Fn]))
  (:require #?(:clj [clojure.core :as core])
            [clojure.string :as string]))

;; state

(defn make-state
  [source]
  {:source source :offset 0})

(defn slice
  ([state] (slice state 1))
  ([{:keys [source offset]} size]
   (when (>= (count source) (core/+ offset size))
     (subs source offset (core/+ offset size)))))

(defn increment-offset
  [state amount]
  (update state :offset core/+ amount))

(defn put-error
  [state message]
  (-> state
      (assoc :error message)
      (dissoc :result state)))

(defn put-result
  [state result]
  (-> state
      (assoc :result result)
      (dissoc :error)))

(defn error?
  [state]
  (contains? state :error))

(defn result?
  [state]
  (not (error? state)))

;; parser protocol

(defprotocol Parser
  (-parse [this state]))

(defmacro pass-state
  {:style/indent 1}
  [state & body]
  `(if (error? ~state)
     ~state
     (do ~@body)))

(defmacro impl-parse
  {:style/indent 1}
  [state & body]
  `(reify Parser
     (-parse [_ ~state]
       (pass-state ~state
         ~@body))))

(defn text
  "Given a string :target return success if it matches."
  [target]
  (impl-parse state
    (let [targetc (count target)
          view    (slice state targetc)]
      (cond
        (nil? view)     (put-error state "EOF")
        (= target view) (increment-offset (put-result state target) targetc)
        :else           (put-error state (str "text: expected " target ", received " view "."))))))

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
            (put-error state "Failed to match all parsers")
            (recur (conj results (:result next)) (rest parsers) next)))))))

(defn alt
  "Given a list of parser, return the result of the first one that succeeds."
  [& parsers]
  (impl-parse state
    (loop [parsers parsers]
      (if (empty? parsers)
        (put-error state "alt: failed to match any parsers")
        (let [next (-parse (first parsers) state)]
          (if (result? next)
            next
            (recur (rest parsers))))))))

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
              (put-error state "Didn't match minimum amount of values")
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
        (put-error state "Sigh")
        (let [parser (get-parser next)
              next (-parse parser next)]
          (if (error? next)
            (put-error state "Sigh")
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
       (fn [result pre post]
         (first result))))

;; interop with core datastructures.
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
            (-parse (apply alt this) state))
          Character
          (-parse [this state]
            (-parse (text (str this)) state))
          String
          (-parse [this state]
            (-parse (text this) state)))
   :cljs (extend-protocol Parser
           string
           (-parse [this state]
             (-parse (text this) state))))

(defn parse
  "A wrapper -parse"
  [parser source]
  (:result (-parse parser (make-state source))))

(defn parse!
  [parser source]
  (let [state (-parse parser (make-state source))]
    (if (error? state)
      (throw (:error state))
      (:result state))))

(defn info
  "Mainly used to debug the result of a parser."
  [parser source]
  (-parse parser (make-state source)))
