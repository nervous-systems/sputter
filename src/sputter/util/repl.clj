(ns sputter.util.repl
  (:require [sputter.util            :as util]
            [sputter.word            :as word]
            [sputter.state.memory    :as mem]
            [clojure.core.rrb-vector :as rrb])
  (:import [java.math        BigInteger]
           [io.nervous.juint UInt256]))

(defmethod print-method BigInteger [word w]
  (.write w (util/bytes->hex word {:pad-left word/size})))

(defmethod print-method UInt256 [word w]
  (.write w (util/bytes->hex word {:pad-left word/size})))

;; (defmethod print-method (type (rrb/vector)) [mem w]
;;   (let [words (mem/stored mem)]
;;     (doseq [i (range words)
;;             :let [[_ v] (mem/recall mem (* i word/size) word/size)]]
;;       (.write w (util/bytes->hex (* i word/size) {:pad-left 2}))
;;       (.write w " ")
;;       (.write w (pr-str (word/->Word v)))
;;       (.write w "\n"))))
