(ns rfc1035.core
  (:require [clojure.string :as str]
            [clojure.set :refer :all]))

; 4.
(defrecord Message [header questions answer-resources authority-resources additional-resources])

; 4.1.1
(defrecord Header [id qr opcode aa tc rd ra z rcode qdcount ancount nscount])

; 4.1.2
(defrecord Question [qname qtype qclass])

; 4.1.3
; rlength will be added during serialization
(defrecord ResourceRecord [name type class ttl rdata])

(defn buff [^String str]
  (vec (.getBytes str)))

(defn byte-to-num
  "Converts a byte buffer into a number"
 [bytes]
  (let [len (count bytes)]
    (cond
      (= len 1) (first bytes)
      (= len 2) (+ (bit-shift-left (first bytes) 3) (second bytes)))))

(defn bit-to-bool
  "Converts a bit into true or false"
  [byte]
  (cond
    (= byte 0) false
    (= byte 1) true))

(defn serialize-domain-name
  [domain-name]
  "Serialize a domain name"
  (concat
    (reduce
      #(concat %1 (list (count %2)) (buff %2))
      (vector)
      (str/split (str/upper-case domain-name) #"\."))
    (list 0)))

(defn pack-num [n len]
  "Converts a number into a byte array with a given padding"
  (let [buff (java.nio.ByteBuffer/allocate len)]
    (vec
      (cond
        (= len 1) [(byte n)]
        (= len 2) (.array (.putShort buff n))))))

(defn pack-str [^String str len]
  "Converts a string into a byte array with a given padding"
  ; TODO https://tools.ietf.org/html/rfc5890 https://tools.ietf.org/html/rfc5891
  (take len (vec (.getBytes str "US-ASCII"))))

; 3.2.4 CLASS values
(def resource-class-map {:in 1 :cs 2 :ch 3 :hs 4})

(defn serialize-resource-class [resource-class]
  (resource-class resource-class-map))

(def resource-type-map {
  ; 3.2.2 TYPES values
  :a     1
  :ns    2
  :md    3
  :mf    4
  :cname 5
  :soa   6
  :mb    7
  :mg    8
  :mr    9
  :null  10
  :wks   11
  :ptr   12
  :hinfo 13
  :minfo 14
  :mx    15
  :txt   16
  :srv   33
})

(def response-code-map {
  :no-error        0
  :format-error    1
  :server-failure  2
  :name-error      3
  :not-implemented 4
  :refused         5
})

(def query-type-map (merge resource-type-map {
  ; 3.2.3 QTYPE values
  :axfr 252
  :mailb 253
  :maila 254
  :* 255
}))

(def opcode-map {:query 0 :iquery 1 :status 2})

(defn serialize-question
  "Serialize a question"
  [question]
  (let [domain-name-buff (serialize-domain-name (:qname question))]
    (concat
      (list (count domain-name-buff))
      domain-name-buff
      (pack-num ((:qtype question) resource-type-map) 2)
      (pack-num ((:qclass question) resource-class-map) 2))))

(defrecord Header [id qr opcode aa tc rd ra rcode qdcount ancount nscount arcount])

; (defn serialize-remote-query
;   "Serialize a remote query"
;   [id question]

(defn serialize-resource-record
  "Serialize a resource record"
  [resource-record])

(defn serialize-header
  "Serialize a header"
  [header]
  (vec (concat
    (pack-num (:id header) 2)
    (pack-num
      (bit-or
        (bit-shift-left (if (:qr header) 1 0) 6)
        (bit-shift-left ((:opcode header) opcode-map) 4)
        (if (:aa header) 8 0)
        (if (:tc header) 4 0)
        (if (:rd header) 2 0)
        (if (:ra header) 1 0))
      1)
    (pack-num
      (bit-and
        ; "Z" part of header reserved for future use
        15
        (:rcode header))
      1)
    (pack-num (:qdcount header) 2)
    (pack-num (:ancount header) 2)
    (pack-num (:nscount header) 2)
    (pack-num (:arcount header) 2))))

(defn make-query-message [id domain-name resource-type resource-class]
  ; 1 question
  (let [header (->Header id 0 :query false false false false 0 1 0 0 0)
        question (->Question domain-name resource-type resource-class)]
    (->Message header [question] [] [] [])))

(defn serialize-message
  [message]
  (vec (flatten
    (list
      (serialize-header (:header message))
      (map serialize-question (:questions message))
      (map serialize-resource-record (:answer-resources message))
      (map serialize-resource-record (:authority-resources message))
      (map serialize-resource-record (:additional-resources message))))))

(defn deserialize-header
  "Deserialize bytes into a message header"
  [header-bytes]
  (let [id      (byte-to-num (subvec header-bytes 0 2))
        qr      (bit-test (get header-bytes 2) 7)
        opcode  (get (map-invert opcode-map)
                  (bit-shift-right
                    (bit-and 7 (get header-bytes 2))
                    4))
        aa      (bit-test (bit-shift-right (get header-bytes 2) 4) 3)
        tc      (bit-test (get header-bytes 2) 2)
        rd      (bit-test (get header-bytes 2) 1)
        ra      (bit-test (get header-bytes 2) 0)
        ; upper 4 bits of 4th byte = Z code and is ignored
        rcode   (get (map-invert response-code-map) (bit-and 15 (get header-bytes 3)))
        qdcount (byte-to-num (subvec header-bytes 4 6))
        ancount (byte-to-num (subvec header-bytes 6 8))
        nscount (byte-to-num (subvec header-bytes 8 10))
        arcount (byte-to-num (subvec header-bytes 10 12))]
    (->Header id qr opcode aa tc rd ra rcode qdcount ancount nscount arcount)))

(defn deserialize-message
  "Deserialize a message into its component parts"
  [message-bytes]
  ; header is 12 bytes
  (let [header (deserialize-header (subvec message-bytes 0 12))]
    (->Message header [] [] [] [])))

; 4.1.4 Message Compression TODO

(defn foo
  "I don't do a whole lot."
  [x]
  (println x "Hello, World!"))
