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
)
