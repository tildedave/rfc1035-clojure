(ns rfc1035.core)

; 4.
(defrecord Message [header question answer authority additional])

; 4.1.1
(defrecord Header [id qr opcode aa tc rd ra z rcode qdcount ancount nscount])

; 4.1.2
(defrecord Question [qname qtype qclass])

; 4.1.3
; rlength will be added during serialization
(defrecord ResourceRecord [name type class ttl rdata])

(defn buff [str]
  (vec (.getBytes str)))

(defn pack-num [n len]
  "Converts a number into a byte array with a given padding"
  (let [buff (java.nio.ByteBuffer/allocate len)]
    (vec (.array
      (cond
        (= len 1) (byte n)
        (= len 2) (.putShort buff n))))))

(defn pack-str [str len]
  "Converts a string into a byte array with a given padding"
  ; TODO https://tools.ietf.org/html/rfc5890 https://tools.ietf.org/html/rfc5891
  (take len (vec (.getBytes str "US-ASCII"))))

(defn serialize-resource-class [resource-class]
  ; 3.2.4 CLASS values
  (resource-class {:in 1 :cs 2 :ch 3 :hs 4}))

(defn serialize-resource-type [resource-type]
  ; 3.2.2 TYPES values
  (resource-type {
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
  }))

(defn serialize-query-type [query-type]
  ; 3.2.3 QTYPE values
  (let [resource-type (serialize-resource-type query-type)]
    (if (some? resource-type) resource-type (query-type {
    :axfr 252
    :mailb 253
    :maila 254
    :* 255
}))))

(defn serialize-question
  "Serialize a question"
  [question]
  (let [domain-name-buff (buff (:qname question))]
    (concat
      (list (count domain-name-buff))
      domain-name-buff
      (pack-num (serialize-resource-type (:qtype question)) 2)
      (pack-num (serialize-resource-class (:qclass question)) 2))))

(defn serialize-resource-record
  "Serialize a resource record"
  [resource-record])

; 4.1.4 Message Compression TODO

(defn foo
  "I don't do a whole lot."
  [x]
  (println x "Hello, World!"))
