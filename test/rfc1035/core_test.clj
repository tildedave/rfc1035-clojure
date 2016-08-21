(ns rfc1035.core-test
  (:require [clojure.test :refer :all]
            [rfc1035.core :refer :all]))

(deftest test-serialize
  (is (= (serialize-resource-type :a)     1))
  (is (= (serialize-resource-type :ns)    2))
  (is (= (serialize-resource-type :cname) 5))
  (is (= (serialize-resource-type :ptr)   12))
  (is (= (serialize-resource-type :mx)    15))
  (is (= (serialize-resource-type :txt)   16))
  (is (= (serialize-resource-type :srv)   33))
  (is (= (serialize-query-type :a)        1))
  (is (= (serialize-query-type :*)        255))
  (is (= (serialize-resource-class :in)      1))
  (is (= (serialize-resource-class :ch)      3))
  ; unclear if questions are correct
  (is (=
    (serialize-domain-name "F.ISI.ARPA")
    '(1 70 3 73 83 73 4 65 82 80 65 0)))
  (is (=
    (serialize-domain-name "F.ISI.ARPA")
    (serialize-domain-name "f.isi.arpa")))
  (is (=
    (serialize-question (->Question "www.google.com" :a :in))
    '(16 3 87 87 87 6 71 79 79 71 76 69 3 67 79 77 0 0 1 0 1)))
  (is (=
    (serialize-question (->Question "www.google.com" :mx :ch))
    '(16 3 87 87 87 6 71 79 79 71 76 69 3 67 79 77 0 0 15 0 3)))
  (is (=
    (serialize-header (->Header 123 0 :query false false false false 0 1 0 0 0))
    '(0 123 8 0 0 0 0 1 0 0 0 0 0 0))))