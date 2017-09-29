(ns sputter.state.storage
  (:require [sputter.word :as word]))

(defprotocol VMStorage
  "Word-addressed storage."
  (retrieve [storage addr pos]
    "Load the word at `pos` from `addr`'s storage.")
  (store [storage addr pos word]
    "Store `word` at `pos` in `addr`'s storage."))

(extend-type (type {})
  VMStorage
  (retrieve [m addr pos]
    (get-in m [addr pos] word/zero))
  (store [m addr pos word]
    (update addr assoc pos word)))

(defn ->mock-Storage [& [m]]
  (or m {}))
