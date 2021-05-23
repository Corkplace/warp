(ns cork.warp
  (:refer-clojure :exclude [+ * map not char])
  (:require #?(:clj [clojure.core :as core])
            [cork.warp.state
             :as state
             :refer [slice increment-offset put-error put-result error? result?]]
            [clojure.string :as string]))

;; parser protocol

(defprotocol Parser
  (-parse [this state]))

(defmacro pass-state
  "Only executes the body when the given state is valid."
  {:style/indent 1}
  [state & body]
  `(if (error? ~state)
     ~state
     (do ~@body)))

(defmacro impl-parse
  "Sets up the boilerplate for creating a parser"
  {:style/indent 1}
  [state & body]
  (let [body body]
    `(reify Parser
       (-parse [_ ~state]
         (pass-state ~state
                     ~@body)))))


(defn text
  "Given a string :target return success if it matches."
  [target]
  (impl-parse state
    (let [targetc (count target)
          view    (slice state targetc)]
      (cond
        (nil? view)     (put-error state {:parser :text
                                          :given  :eof})
        (= target view) (increment-offset (put-result state target) targetc)
        :else           (put-error state {:parser   :text
                                          :expected target
                                          :given    view})))))

(def eof
  "Matches end of source."
  (impl-parse state
    (let [{:keys [offset source]} state]
      (if (>= offset (count source))
        (put-result state :eof)
        (put-error state {:parser :eof})))))

(def bof
  "Matches begining of source."
  (impl-parse state
    (let [{:keys [offset source]} state]
      (if (= 0 offset)
        (put-result state :eof)
        (put-error state {:parser :eof})))))

(def one
  "Matches any one character slice."
  (impl-parse state
    (let [value (slice state)]
      (if (nil? value)
        (put-error state {:parser :match-one
                          :given  :eof})
        (increment-offset (put-result state value))))))

(defn parse
  "A wrapper -parse"
  [parser source]
  (:result (-parse parser (state/make source))))

(defn parse!
  [parser source]
  (let [state (-parse parser (state/make source))]
    (if (error? state)
      (throw (:error state))
      (:result state))))

(defn info
  "Mainly used to debug the result of a parser."
  [parser source]
  (-parse parser (state/make source)))

;;
;; interop with core datastructures. kinda cool.
;;

#?(:clj (extend-protocol Parser
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
