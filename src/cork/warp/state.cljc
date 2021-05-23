(ns cork.warp.state)

(defn make
  [source]
  {:source source :offset 0})

(defn slice
  ([state] (slice state 1))
  ([{:keys [source offset]} size]
   (when (>= (count source) (+ offset size))
     (subs source offset (+ offset size)))))

(defn increment-offset
  ([state] (increment-offset state 1))
  ([state amount]
   (update state :offset + amount)))

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
