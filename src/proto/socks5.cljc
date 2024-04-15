(ns http.socks5
  (:require [ioutil.bytes :as bytes]
            [ioutil.struct :as struct]
            [ioutil.structs :as structs]
            [ioutil.stream :as stream]
            [proto.addr :as addr]))

;; RFC1928: SOCKS5
;; RFC1929: SOCKS5 password auth

(struct/defstruct socks5-ver [:struct :spec structs/uint8 :to-validate (= it 5)])
(struct/defstruct socks5-rsv [:struct :spec structs/uint8 :to-validate (= it 1)])

(struct/defstruct socks5-str
  [:vbytes
   :len 1
   :to (bytes/bytes->str it)
   :from (bytes/str->bytes it)])

;;; 3. Procedure for TCP-based clients

(def ^:const AUTH-NO-REQUIRED   0x00)
(def ^:const AUTH-GSSAPI        0x01)
(def ^:const AUTH-PASSWORD      0x02)
(def ^:const AUTH-NO-ACCEPTABLE 0xff)

(struct/defstruct socks5-auth-request
  [[ver socks5-ver]
   [methods [:vlist :len 1 :spec structs/uint8]]])

(struct/defstruct socks5-auth-reply
  [[ver socks5-ver]
   [method structs/uint8]])

;;; RFC1929 Username/Password Auth for SOCKS5

(def ^:const PAUTH-SUCCEED 0)

(struct/defstruct socks5-pauth-request
  [[ver socks5-ver]
   [uname socks5-str]
   [passwd socks5-str]])

(struct/defstruct socks5-pauth-reply
  [[ver socks5-ver]
   [status structs/uint8]])

;;; 4. Requests

(def ^:const CMD-CONNECT   0x01)
(def ^:const CMD-BIND      0x02)
(def ^:const CMD-UDP-ASSOC 0x03)

(def ^:const ATYP-DOMAIN 0x03)
(def ^:const ATYP-IPV4   0x01)
(def ^:const ATYP-IPV6   0x04)

(struct/defstruct socks5-addr
  [[atyp structs/uint8]
   [addr [:case
          :tests [atyp
                  ATYP-DOMAIN socks5-str
                  ATYP-IPV4 addr/ipv4
                  ATYP-IPV6 addr/ipv6]]]
   [port structs/uint16be]])

(struct/defstruct socks5-request
  [[ver socks5-ver]
   [cmd structs/uint8]
   [rsv socks5-rsv]
   [[atyp addr port] socks5-addr]])

;;; 6. Replies

(def ^:const REP-SUCCEEDED          0x00)
(def ^:const REP-FAILURE            0x01)
(def ^:const REP-NOT-ALLOWED        0x02)
(def ^:const REP-NET-UNREACH        0x03)
(def ^:const REP-HOST-UNREACH       0x04)
(def ^:const REP-CONN-REFUSED       0x05)
(def ^:const REP-TTL-EXPIRED        0x06)
(def ^:const REP-CMD-NOT-SUPPORTED  0x07)
(def ^:const REP-ATYP-NOT-SUPPORTED 0x08)

(struct/defstruct socks5-reply
  [[ver socks5-ver]
   [rep structs/uint8]
   [rsv socks5-rsv]
   [[atyp addr port] socks5-addr]])

;;; streams

(defn socks5-wrap-stream [source putfn host port]
  (let [ring (stream/make-ring source)]
    ))
