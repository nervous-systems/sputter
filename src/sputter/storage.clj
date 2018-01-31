(ns sputter.storage)

(defprotocol VMStorage
  "Word-addressed storage, keyed by Ethereum address."
  (retrieve [storage addr pos]
    "Load the word at `pos` from `addr`'s storage.")
  (store [storage addr pos word]
    "Store `word` at `pos` in `addr`'s storage.")
  (stored [storage addr]
    "Return the number of words in `addr`'s storage."))
