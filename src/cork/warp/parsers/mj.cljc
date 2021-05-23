(ns cork.warp.parsers.mj
  (:require
   [clojure.java.io :as io]
   [cork.warp :as w]
   [cork.warp.combinators :as c]
   [cork.warp.text :as t]
   [cork.warp.util :as u]))

(let [ordinary-symbols (t/char-of "-,.?")
      special-symbols  (t/char-of "/*~_\\")
      newline          #{"\r\n" "\n"}
      escape           (-> ["\\" #{special-symbols newline}]
                           (c/map (fn [[_ value] _ _] value)))
      ws+              (c/+ " ")
      character        (c/alt t/letter " " ordinary-symbols escape)
      text             (t/join (w/+ character))
      modifier         (fn [node-name delim]
                         (-> [delim text delim]
                             (w/map (fn [[_ content _] _ _]
                                      {:op      node-name
                                       :content content}))))
      italic           (modifier :italic "/")
      bold             (modifier :bold "*")
      underline        (modifier :underline "_")
      strikethrough    (modifier :strikethrough "~")
      content          (c/alt italic bold underline strikethrough (c/map text
                                                                         (fn [result _ _]
                                                                           {:op :text :content result})))
      header           (-> [(c/repeated "#" :from 1 :to 6) ws+ (c/* content)]
                           (c/map (fn [[h _ content] _ _]
                                    {:op      :header
                                     :level   (count h)
                                     :content content})))
      paragraph        (-> (c/* content)
                           (c/map (fn [content _ _]
                                    {:op      :paragraph
                                     :content content})))
      page             (-> [(u/sep-by (w/alt header paragraph) (c/+ "\n")) w/eof]
                           (c/map (fn [[page-content] _ _]
                                    {:op      :page
                                     :content page-content})))]
  (def parser page))
