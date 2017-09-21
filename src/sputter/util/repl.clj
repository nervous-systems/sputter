(ns sputter.util.repl
  (:require [sputter.util :as util]
            [sputter.word :as word])
  (:import [sputter.state.memory Memory]
           [java.math BigInteger]))

(defmethod print-method BigInteger [word w]
  (.write w (util/bytes->hex word {:pad-left word/size})))

(defmethod print-method Memory [mem w]
  (doseq [word (vals (:table mem))]
    (.write w (pr-str word))
    (.write w "\n")))
