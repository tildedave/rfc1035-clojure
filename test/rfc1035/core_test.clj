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
  (is (= (serialize-question (->Question "www.google.com" :a :in))
    '(14 119 119 119 46 103 111 111 103 108 101 46 99 111 109 0 1 0 1)))
  (is (= (serialize-question (->Question "www.google.com" :mx :ch))
    '(14 119 119 119 46 103 111 111 103 108 101 46 99 111 109 0 15 0 3)))
  )
