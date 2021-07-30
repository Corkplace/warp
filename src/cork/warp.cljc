(ns cork.warp
  (:refer-clojure :exclude [+ * map not char])
  (:require
    [cork.warp.state :as state :refer [slice increment-offset put-error put-result error? result?]]
    [cork.warp.macros :as m]))

(defn literal
  "Given a string :target return success if it matches."
  [target]
  (m/impl-parse state
    (let [targetc (count target)
          view (slice state targetc)]
      (cond
        (nil? view) (put-error state {:parser :text
                                      :given  :eof})
        (= target view) (increment-offset (put-result state target) targetc)
        :else (put-error state {:parser   :text
                                :expected target
                                :given    view})))))

(def eof
  "Matches end of source."
  (m/impl-parse state
    (let [{:keys [offset source]} state]
      (if (>= offset (count source))
        (put-result state :eof)
        (put-error state {:parser :eof})))))

(def bof
  "Matches beginning of source."
  (m/impl-parse state
    (let [{:keys [offset source]} state]
      (if (= 0 offset)
        (put-result state :eof)
        (put-error state {:parser :eof})))))

(def one
  "Matches any one character slice."
  (m/impl-parse state
    (let [value (slice state)]
      (if (nil? value)
        (put-error state {:parser :match-one
                          :given  :eof})
        (increment-offset (put-result state value))))))

(defn parse
  "A wrapper -parse"
  [parser source]
  (:result (state/-parse parser (state/make source))))

(defn parse!
  [parser source]
  (let [state (state/-parse parser (state/make source))]
    (if (error? state)
      (throw (:error state))
      (:result state))))

(defn info
  "Mainly used to debug the result of a parser."
  [parser source]
  (state/-parse parser (state/make source)))

;;
;; interop with core datastructures. kinda cool.
;;

#?(:clj  (extend-protocol state/Parser
           Character
           (-parse [this state]
             (state/-parse (literal (str this)) state))
           String
           (-parse [this state]
             (state/-parse (literal this) state)))
   :cljs (extend-protocol state/Parser
           string
           (-parse [this state]
             (state/-parse (literal this) state))))
