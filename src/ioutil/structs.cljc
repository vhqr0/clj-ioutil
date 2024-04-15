(ns ioutil.structs
  (:require [ioutil.bytes :as bytes]
            [ioutil.struct :as struct]))

(struct/defstruct uint8 [:int :len 1])
(struct/defstruct sint8 [:int :len 1 :signed true])

(struct/defstruct uint16be [:int :len 2])
(struct/defstruct sint16be [:int :len 2 :signed true])
(struct/defstruct uint16le [:int :len 2 :order :little])
(struct/defstruct sint16le [:int :len 2 :order :little :signed true])

(struct/defstruct uint32be [:int :len 4])
(struct/defstruct sint32be [:int :len 4 :signed true])
(struct/defstruct uint32le [:int :len 4 :order :little])
(struct/defstruct sint32le [:int :len 4 :order :little :signed true])

(struct/defstruct uint64be [:int :len 8])
(struct/defstruct sint64be [:int :len 8 :signed true])
(struct/defstruct uint64le [:int :len 8 :order :little])
(struct/defstruct sint64le [:int :len 8 :order :little :signed true])

(struct/defstruct lf-line [:line :end "\n"])
(struct/defstruct cr-line [:line :end "\r"])
(struct/defstruct crlf-line [:line :end "\r\n"])
