(ns cork.warp.json
  (:require
   [cork.warp :as w]
   [cork.warp.util :as u]
   [cork.warp.text :as t]))


(def ws (w/+ #{\u0020 \u000A \u000D \u0009}))

(def sign #{"+" "-"})
