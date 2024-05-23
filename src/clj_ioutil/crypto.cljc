(ns clj-ioutil.crypto
  (:require [clojure.set :as set]
            [promesa.core :as p]
            [clj-ioutil.bytes :as b]
            [clj-ioutil.crypto.impl :as impl]))

;;; digest

(def digest-algos #{:sha256 :sha384 :sha512})

(def digest-size
  {:sha256 32
   :sha384 48
   :sha512 64})

(def digest-block-size
  {:sha256  64
   :sha384 128
   :sha512 128})

;; key: bytes or platform native secret key
(def digest "[data algo]" impl/digest)
(def hmac "[data key algo]" impl/hmac)

;;; cipher

(def aead-algos #{:aes128-gcm :aes192-gcm :aes256-gcm :chacha20-poly1305})

(def aes-algos #{:aes128-cbc :aes192-cbc :aes256-cbc :aes128-ctr :aes192-ctr :aes256-ctr})

(def cipher-algos (set/union aead-algos aes-algos))

(def cipher-key-size
  {:aes128-cbc        16
   :aes192-cbc        24
   :aes256-cbc        32
   :aes128-ctr        16
   :aes192-ctr        24
   :aes256-ctr        32
   :aes128-gcm        16
   :aes192-gcm        24
   :aes256-gcm        32
   :chacha20-poly1305 32})

(def cipher-iv-size
  {:aes128-cbc        16
   :aes192-cbc        16
   :aes256-cbc        16
   :aes128-ctr        16
   :aes192-ctr        16
   :aes256-ctr        16
   :aes128-gcm        12
   :aes192-gcm        12
   :aes256-gcm        12
   :chacha20-poly1305 12})

(def aead-tag-size
  {:aes128-gcm        16
   :aes192-gcm        16
   :aes256-gcm        16
   :chacha20-poly1305 16})

;; key: bytes or platform native secret key
(def generate-key "[algo]" impl/generate-key)
(def bytes->key "[data algo]" impl/bytes->key)
(def key->bytes "[key algo]" impl/key->bytes)
(def encrypt "[data key iv algo & {:keys [aad]}]" impl/encrypt)
(def decrypt "[data key iv algo & {:keys [aad]}]" impl/decrypt)

;;; pk

(def ke-algos #{:ecdh-p256 :ecdh-p384 :ecdh-p521 :x25519 :x448})

(def sign-algos
  #{:ecdsa-p256-sha256
    :ecdsa-p256-sha384
    :ecdsa-p256-sha512
    :ecdsa-p384-sha256
    :ecdsa-p384-sha384
    :ecdsa-p384-sha512
    :ecdsa-p521-sha256
    :ecdsa-p521-sha384
    :ecdsa-p521-sha512
    :ed25519
    :ed448})

(def pk-algos (set/union ke-algos sign-algos))

(def ke-key-size
  {:ecdh-p256 48
   :ecdh-p384 48
   :ecdh-p521 48
   :x25519    32
   :x448      56})

(def generate-keypair "[algo]" impl/generate-keypair)
(def bytes->pri "[data algo]" impl/bytes->pri)
(def bytes->pub "[data algo]" impl/bytes->pub)
(def pri->bytes "[pri]" impl/pri->bytes)
(def pub->bytes "[pub]" impl/pub->bytes)
(def key-exchange "[pub pri algo & {:keys [size]}]" impl/key-exchange)
(def sign "[data pri algo]" impl/sign)
(def verify "[data sig pub algo]" impl/verify)

;;; test

(comment
  (do
    (def res (atom nil))
    (-> (p/let [m (digest (b/str->bytes "hello") :sha384)]
          (reset! res m))
        (p/catch #(reset! res %))))
  (do
    (def res (atom nil))
    (-> (p/let [m (hmac (b/str->bytes "hello") (b/rand-bytes 4) :sha384)]
          (reset! res m))
        (p/catch #(reset! res %)))))

(comment
  (def a :aes256-cbc)
  (def a :aes256-ctr)
  (def a :aes256-gcm)
  (def a :chacha20-poly1305)
  (do
    (def res (atom nil))
    (-> (p/let [k (generate-key a)]
          (reset! res k))
        (p/catch #(reset! res %))))
  (do
    (def k @res)
    (reset! res nil)
    (-> (p/let [d (key->bytes k a)]
          (reset! res d))
        (p/catch #(reset! res %))))
  (do
    (reset! res nil)
    (def iv (b/rand-bytes (cipher-iv-size a)))
    (def d (b/str->bytes "hello"))
    (-> (p/let [e (encrypt d k iv a)]
          (reset! res e))
        (p/catch #(reset! res %))))
  (do
    (def e @res)
    (reset! res nil)
    (-> (p/let [d (decrypt e k iv a)]
          (reset! res (b/bytes->str d)))
        (p/catch #(reset! res %)))))

(comment
  (def a :ecdh-p384)
  (def a :ecdh-p521)
  (def a :x448)
  (do
    (def res (atom nil))
    (-> (p/let [kp (generate-keypair a)]
          (reset! res kp))
        (p/catch #(reset! res %))))
  (do
    (def pri (first @res))
    (def pub (last @res)))
  ;; test pri fac
  (do
    (reset! res nil)
    (-> (p/let [d (pri->bytes pri a)]
          (reset! res d))
        (p/catch #(reset! res %))))
  (do
    (-> (p/let [k (bytes->pri @res a)]
          (reset! res k))
        (p/catch #(reset! res %))))
  ;; test pub fac
  (do
    (reset! res nil)
    (-> (p/let [d (pub->bytes pub a)]
          (reset! res d))
        (p/catch #(reset! res %))))
  (do
    (-> (p/let [k (bytes->pub @res a)]
          (reset! res k))
        (p/catch #(reset! res %))))
  ;; test ke
  (do
    (reset! res nil)
    (-> (p/let [k (key-exchange pub pri a)]
          (reset! res k))
        (p/catch #(reset! res nil)))))

(comment
  (def a :ecdsa-p384-sha512)
  (def a :ecdsa-p521-sha384)
  (def a :ed448)
  (do
    (def res (atom nil))
    (-> (p/let [kp (generate-keypair a)]
          (reset! res kp))
        (p/catch #(reset! res %))))
  (do
    (def pri (first @res))
    (def pub (last @res))
    (def d (b/str->bytes "hello")))
  ;; test sign
  (do
    (reset! res nil)
    (-> (p/let [s (sign d pri a)]
          (reset! res s))
        (p/catch #(reset! res %))))
  (do
    (def s @res)
    (reset! res nil)
    (-> (p/let [ok (verify d s pub a)]
          (reset! res ok))
        (p/catch #(reset! res %)))))
