(ns sputter.state.memory
  (:require [sputter.word :as word]
            [sputter.util :as util]
            [sputter.util.biginteger :as b])
  (:import [java.util Arrays])
  (:refer-clojure :exclude [load]))

(defprotocol VMMemory
  (store      [mem pos word])
  (store-byte [mem pos b])
  (load       [mem pos]))

(defn- apply-table [mem pos f & args]
  (apply update-in mem [:table pos] (fnil f word/zero) args))

(defrecord Memory [table]
  VMMemory
  (store [mem pos word]
    (let [slot    (quot pos word/size)
          overlap (rem  pos word/size)
          excess  (- word/size overlap)]
      (-> mem
          (apply-table slot word/join word excess)
          (cond-> (not (zero? overlap))
            (apply-table (inc slot) #(word/join word % excess))))))
  (store-byte [mem pos b]
    (let [slot     (quot pos word/size)
          word-pos (rem  pos word/size)]
      (apply-table mem slot word/insert b word-pos)))
  (load [mem pos]
    (table (quot pos word/size) word/zero)))

(def memory? (partial satisfies? VMMemory))

(defn ->Memory [x]
  (cond
    (memory? x) x
    (map?    x) (Memory. (into (sorted-map) x))))
