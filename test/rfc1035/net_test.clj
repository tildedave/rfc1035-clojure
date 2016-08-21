(ns rfc1035.net-test
  (:require [clojure.test :refer :all]
            [rfc1035.net :refer :all]
            [rfc1035.core :refer :all]))

(import '[java.net DatagramSocket])

(defn test-query
  [message]
  (let [socket (DatagramSocket. 0)]
    (try
      (do
        (.setSoTimeout socket 5000)
        (udp-send socket (byte-array (serialize-message message)) "localhost" 53)
        (vec (udp-receive socket)))
      (finally (.close socket)))))

(deftest live-test
  (let [response (test-query (make-query-message "www.google.com" :a :in))
        message  (deserialize-message response)]
    (is (= (:qr    (:header message)) true)      "Response bit was set")
    (is (= (:rcode (:header message)) :no-error) "No error in response")
    (is (= (:authority-resources message) true)     "Show me what you think the resources are")
    ))