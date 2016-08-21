(ns rfc1035.core-test
  (:require [clojure.test :refer :all]
            [rfc1035.core :refer :all]))

(deftest test-serialize
  (is (= (resource-type-map :a)     1))
  (is (= (resource-type-map :ns)    2))
  (is (= (resource-type-map :cname) 5))
  (is (= (resource-type-map :ptr)   12))
  (is (= (resource-type-map :mx)    15))
  (is (= (resource-type-map :txt)   16))
  (is (= (resource-type-map :srv)   33))
  (is (= (query-type-map :a)        1))
  (is (= (query-type-map :*)        255))
  (is (= (resource-class-map :in)   1))
  (is (= (resource-class-map :ch)   3))
  ; unclear if questions are correct
  (is (=
    (serialize-domain-name "F.ISI.ARPA")
    '(0 1 70 0 3 73 83 73 0 4 65 82 80 65 0)))
  (is (=
    (serialize-domain-name "F.ISI.ARPA")
    (serialize-domain-name "f.isi.arpa")))
  (is (=
    (serialize-question (->Question "www.google.com" :a :in))
    '(0 3 87 87 87 0 6 71 79 79 71 76 69 0 3 67 79 77 0 0 1 0 1)))
  (is (=
    (serialize-question (->Question "www.google.com" :mx :ch))
    '(0 3 87 87 87 0 6 71 79 79 71 76 69 0 3 67 79 77 0 0 15 0 3)))
  (is (=
    (count (serialize-header (->Header 123 0 :query false false false false 0 1 0 0 0)))
    12)
    "Header serialized to expected length")
  (is (=
    (serialize-header (->Header 123 0 :query false false false false 0 1 0 0 0))
    '(0 123 128 0 0 1 0 0 0 0 0 0))
    "Header is as expected"))