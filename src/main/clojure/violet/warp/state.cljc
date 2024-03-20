(ns violet.warp.state
  (:refer-clojure :exclude [peek pop]))

(defprotocol Parser
  (-parse [this state]))

(defn make
  [source]
  {:source (seq source) :offset 0})

(defn peek
  [state]
  ((comp first :source) state))

(defn pop
  [state]
  (-> state
      (update :source rest)
      (update :offset inc)))

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

