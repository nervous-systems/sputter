(ns sputter.vm-test
  (:require [clojure.test :refer [deftest]]
            [sputter.test.util :as test.util] :reload))

(deftest arithmetic-add
  (test.util/run-vm-tests #"^add\d+$"))

(deftest arithmetic-sub
  (test.util/run-vm-tests #"^sub\d+$"))

(deftest pushdupswap-dup
  (test.util/run-vm-tests #"^dup"))

(deftest pushdupswap-push
  (test.util/run-vm-tests #"^push(?!32AndSuicide)"))
