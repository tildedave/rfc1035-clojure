(ns rfc1035.core
  (:require [clojure.string :as str]
            [clojure.set :refer :all]))


; https://www.ietf.org/rfc/rfc1035.txt

; 4.
(defrecord Message [header questions answer-resources authority-resources additional-resources])

; 4.1.1
(defrecord Header [id is-response opcode aa tc rd ra z rcode qdcount ancount nscount])

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

(defn pack-num [n len]
  "Converts a number into a byte array with a given padding"
  (vec
    (cond
      ; java bytes are signed by default
      (= len 1) [(bit-and n 0xFF)]
      ; can't use java buffers because they're all signed :(
      (= len 2) [(bit-and (bit-shift-left n 8) 0xFF) (bit-and n 0xFF)])))

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

(defn serialize-domain-name
  [domain-name]
  "Serialize a domain name"
  (concat
    (reduce
      ; octet length is limited to 63 characters (first two bits used for
      ; message compression)
      #(concat %1 (pack-num (bit-and 0x3F (count %2)) 2) (buff %2))
      (vector)
      (str/split (str/upper-case domain-name) #"\."))
    (list 0)))

(defn serialize-question
  "Serialize a question"
  [question]
  (let [domain-name-buff (serialize-domain-name (:qname question))]
    (concat
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
        (if (true? (:is-response header)) 0x80 0)
        (bit-shift-left ((:opcode header) opcode-map) 4)
        (if (true? (:aa header)) 8 0)
        (if (true? (:tc header)) 4 0)
        (if (true? (:rd header)) 2 0)
        (if (true? (:ra header)) 1 0))
      1)
    (pack-num
      (bit-and
        ; "Z" part of header reserved for future use
        0x0F
        (:rcode header))
      1)
    (pack-num (:qdcount header) 2)
    (pack-num (:ancount header) 2)
    (pack-num (:nscount header) 2)
    (pack-num (:arcount header) 2))))

(defn make-query-message [id domain-name resource-type resource-class]
  ; 1 question
  (let [header (->Header id false :query false false false false 0 1 0 0 0)
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

(defn deserialize-labels
  [message-bytes offset labels]
  (cond
    (= (byte-to-num (subvec message-bytes offset (+ offset 2))) 0)
      {:label-parts labels :offset (inc offset)}
    (> (bit-and 0xC (get message-bytes offset)) 0)
      ; message compression
      nil
    :else
      (let [label-offset (+ offset 2)
            label-len    (byte-to-num (subvec message-bytes offset label-offset))
            label-end    (+ label-offset label-len)
            label-part   (String. (byte-array (subvec message-bytes label-offset label-end)))]
        (recur message-bytes label-end (conj labels label-part)))))

(defn deserialize-question
  "Deserialize a question from message bytes."
  [message-bytes offset]
  (let [labels    (deserialize-labels message-bytes offset [])
        label-str (str/join "." (:label-parts labels))
        label-end (:offset labels)
        qtype     (get (map-invert query-type-map)
                       (byte-to-num (subvec message-bytes label-end (+ label-end 2))))
        qclass    (get (map-invert resource-class-map)
                       (byte-to-num (subvec message-bytes (+ label-end 2) (+ label-end 4))))]
    {:question (->Question label-str qtype qclass)
     :offset   (+ label-end 4)}))
      ; label-end
      ; (byte-to-num (subvec message-bytes label-end (+ label-end 2))))))

(defn deserialize-questions
  [message-bytes offset questions num-questions]
  (if
    (= num-questions 0) {:questions questions :offset offset}
    (let [question (deserialize-question message-bytes offset)]
      (recur message-bytes
        (:offset question)
        (conj questions (:question question))
        (dec num-questions)))))

(defn deserialize-message
  "Deserialize a message into its component parts"
  [message-bytes]
  ; header is 12 bytes
  (let [header        (deserialize-header (subvec message-bytes 0 12))
        questions     (deserialize-questions message-bytes 12 [] (:qdcount header))]
    (->Message header (:questions questions) [] [] [])))

; 4.1.4 Message Compression TODO

(defn foo
  "I don't do a whole lot."
  [x]
  (println x "Hello, World!"))
