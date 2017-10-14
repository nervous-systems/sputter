(ns sputter.vm-test
  (:require [clojure.test :refer [deftest]]
            [sputter.test.util :as test.util] :reload))

(deftest add
  (test.util/run-vm-tests #"^add\d+$"))

(deftest sub
  (test.util/run-vm-tests #"^sub\d+$"))

(deftest dup
  (test.util/run-vm-tests #"^dup"))

(deftest swap
  (test.util/run-vm-tests #"^swap"))

(deftest push
  (test.util/run-vm-tests #"^push(?!32AndSuicide)"))

(deftest mload
  (test.util/run-vm-tests #"^mload"))

(deftest mstore
  (test.util/run-vm-tests #"^mstore"))
