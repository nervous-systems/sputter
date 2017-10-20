(ns sputter.word.int128-test
  (:require [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [sputter.util.biginteger :as b]
            [sputter.word.int256]
            [sputter.word.int128     :as i]
            [sputter.word.fixed      :as f])
  (:import [java.math BigInteger]))

(def g-int128
  (gen/fmap
   (fn [[a b]]
     (i/->int128 a b))
   (gen/tuple gen/large-integer
              gen/large-integer)))

(defn truncate [^BigInteger b]
  (b/mask b 128))

(defspec biginteger-roundtrip
  (prop/for-all [x g-int128]
    (zero? (compare x (i/biginteger->int128 (f/->biginteger x))))))

(defspec add-equivalence
  (prop/for-all [x g-int128
                 y g-int128]
    (= (truncate (b/+ (f/->biginteger x)
                      (f/->biginteger y)))
       (f/->biginteger (f/add x y)))))

(defspec add-sub
  (prop/for-all [x g-int128
                 y g-int128]
    (zero? (compare x (f/sub (f/add x y) y)))))

(defspec mul-equivalence
  (prop/for-all [x g-int128
                 y g-int128]
    (= (truncate (b/* (f/->biginteger x)
                      (f/->biginteger y)))
       (f/->biginteger (f/mul x y)))))

(defspec lshift-equivalence
  (prop/for-all [x     g-int128
                 shift gen/pos-int]
    (= (truncate (b/<< (f/->biginteger x) shift))
       (f/->biginteger (f/lshift x shift)))))

(defspec rshift-equivalence
  (prop/for-all [x     g-int128
                 shift gen/pos-int]
    (= (truncate (b/>> (f/->biginteger x) shift))
       (f/->biginteger (f/rshift x shift)))))

(defspec bit-count-equivalence
  (prop/for-all [x g-int128]
    (= (.bitCount (f/->biginteger x))
       (f/bit-count x))))

(defspec bit-length-equivalence
  (prop/for-all [x g-int128]
    (= (.bitLength (f/->biginteger x))
       (f/bit-length x))))

(defspec zero?-equivalence
  (prop/for-all [x g-int128]
    (= (= 0 (f/->biginteger x)) (f/zero? x))))

(defspec bit-and-equivalence
  (prop/for-all [x g-int128
                 y g-int128]
    (= (truncate (b/and (f/->biginteger x) (f/->biginteger y)))
       (f/->biginteger (f/bit-and x y)))))

(defspec divide-equivalence 1000
  (prop/for-all [x g-int128
                 y (gen/such-that (complement f/zero?) g-int128)]
    (= (truncate (b// (f/->biginteger x) (f/->biginteger y)))
       (-> (f/div x y) f/->biginteger))))
