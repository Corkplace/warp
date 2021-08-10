(ns cork.warp.macros
  (:require [cork.warp.state :as state]
            [clojure.pprint :refer [pprint]]))

(defn -pass-state [state body]
  `(if (state/error? ~state)
     ~state
     (do ~@body)))

(defmacro pass-state
  "Only executes the body when the given state is valid."
  {:style/indent 1}
  [state & body]
  (-pass-state state body))

(defn -impl-parse [state body]
  (let [-parse '-parse
        this '_]
    `(reify state/Parser
       (~-parse [~this ~state]
         ~(-pass-state state body)))))

(defmacro impl-parse
  "Sets up the boilerplate for creating a parser"
  {:style/indent 1}
  [state & body]
  (let [result (-impl-parse state body)]
    result))
