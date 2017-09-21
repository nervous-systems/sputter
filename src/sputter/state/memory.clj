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

(defn- update* [mem pos f & args]
  (apply update-in mem [:table pos] (fnil f word/zero) args))

(defrecord Memory [table]
  VMMemory
  (store [mem pos word]
    (let [slot    (quot pos word/size)
          in-slot (- word/size (rem pos word/size))]
      (-> mem
          (update* slot word/join word in-slot)
          (cond-> (< in-slot word/size)
            (update* (inc slot) #(word/join word % in-slot))))))
  (store-byte [mem pos b]
    (let [slot (quot pos word/size)]
      (update* mem slot word/insert b (rem pos word/size))))
  (load [mem pos]
    (let [slot (quot pos word/size)]
      (word/join
       (table slot       word/zero)
       (table (inc slot) word/zero)
       (rem pos word/size)))))

(def memory? (partial satisfies? VMMemory))

(defn ->Memory [x]
  (cond
    (memory? x) x
    (map?    x) (Memory. (into (sorted-map) x))))
