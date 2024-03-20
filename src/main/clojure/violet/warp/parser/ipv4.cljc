(ns violet.warp.parser.ipv4
  (:require [violet.warp.combinators :as c]
            [violet.warp.state :as s]
            [violet.warp.text :as t]
            [violet.warp :as w]))

(def ^:private dot \.)
(def ^:private z5 (t/char-of "012345"))
(def ^:private z6 (t/char-of "123456"))

;; 0 - 256

(def oct
  (c/map
   (fn [result _ _]
     (Integer/parseInt result))
   (t/compact
    (c/alt [[\1 t/digit t/digit]
            [\2 z5 z6]
            [t/digit t/digit]
            [t/digit]]))))

(defrecord Ipv4 [o1 o2 o3 o4])

(def v4
  (c/map
   (fn [[o1 _ o2 _ o3 _ o4] _ _]
     (->Ipv4 o1 o2 o3 o4))
   [oct \. oct \. oct \. oct]))

(w/info v4 "192.168.1.34")
