(ns rfc1035.net
  (:require [rfc1035.core :refer :all]))

(import '[java.net DatagramSocket
                   DatagramPacket
                   InetSocketAddress])

(defn udp-send
  "Send a short textual message over a DatagramSocket to the specified
  host and port. If the string is over 512 bytes long, it will be
  truncated."
  [^DatagramSocket socket payload host port]
  (let [length (count payload)
        address (InetSocketAddress. host port)
        packet (DatagramPacket. payload length address)]
    (.send socket packet)))

(defn udp-receive
  "Block until a UDP message is received on the given DatagramSocket, and
  return the payload message as a string."
  [^DatagramSocket socket]
  (let [buffer (byte-array 512)
        packet (DatagramPacket. buffer 512)]
    (.receive socket packet)
    (byte-array (.getData packet))))
