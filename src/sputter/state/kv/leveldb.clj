(ns sputter.state.kv.leveldb
  (:refer-clojure :exclude [set])
  (:require [sputter.util :refer [sha3-bytes] :rename {sha3-bytes sha3}]
            [clojure.java.io  :as java.io]
            [sputter.state.kv :as kv])
  (:import [org.iq80.leveldb.impl Iq80DBFactory]
           [org.iq80.leveldb Options DB]))

(defprotocol LevelDBCodec
  (outgoing-key   [_ k])
  (incoming-key   [_ k])
  (outgoing-value [_ v])
  (incoming-value [_ v]))

(extend-type nil
  LevelDBCodec
  (outgoing-key   [_ k] k)
  (incoming-key   [_ k] k)
  (outgoing-value [_ v] v)
  (incoming-value [_ v] v))

(defrecord LevelDBStore [db codec]
  kv/KeyValueStore
  (insert [this k v]
    (.put db (outgoing-key codec k) (outgoing-value codec v))
    this)
  (insert-batch [this m]
    (let [batch (.createWriteBatch db)]
      (try
        (doseq [[k v] m]
          (.put batch (outgoing-key codec k) (outgoing-value codec v)))
        (.write db batch)
        (finally
          (.close batch))))
    this)
  (retrieve [this k]
    (incoming-value codec (.get db (outgoing-key codec k))))
  (delete [this k]
    (.delete db (outgoing-key codec k))
    this))

(defn map->LevelDBStore [opts]
  (let [options (doto (Options.)
                  (.createIfMissing true))
        f       (java.io/file (:path opts))
        db      (.open Iq80DBFactory/factory f options)]
    (LevelDBStore. db (:codec opts))))
