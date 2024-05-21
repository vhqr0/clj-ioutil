(ns clj-ioutil.crypto
  (:require [clj-ioutil.crypto.impl :as impl]))

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
(def bytes->digest "[data algo]" impl/bytes->digest)
(def bytes->hmac "[data key algo]" impl/bytes->hmac)

(comment
  (do
    (def res (atom nil))
    (-> (promesa.core/let [m (bytes->digest (clj-ioutil.bytes/str->bytes "hello") :sha384)]
          (reset! res m))
        (promesa.core/catch #(reset! res %))))
  (do
    (def res (atom nil))
    (-> (promesa.core/let [m (bytes->hmac (clj-ioutil.bytes/str->bytes "hello") (clj-ioutil.bytes/rand-bytes 4) :sha384)]
          (reset! res m))
        (promesa.core/catch #(reset! res %)))))

;;; aead

(def aead-algos #{:aes128gcm :aes256gcm :chacha20poly1305})

(def aead-key-size
  {:aes128gcm 16
   :aes256gcm 32
   :chacha20poly1305 32})

(def aead-iv-size
  {:aes128gcm 12
   :aes256gcm 12
   :chacha20poly1305 12})

(def aead-tag-size
  {:aes128gcm 16
   :aes256gcm 16
   :chacha20poly1305 16})

;; key: bytes or platform native secret key
(def aead-key->bytes "[key algo]" impl/aead-key->bytes)
(def bytes->aead-key "[data algo]" impl/bytes->aead-key)
(def aead-encrypt "[data key iv algo & {:keys [aad]}]" impl/aead-encrypt)
(def aead-decrypt "[data key iv algo & {:keys [aad]}]" impl/aead-decrypt)

(comment
  (def a :aes256gcm)
  (def a :chacha20poly1305)
  (do
    (def res (atom nil))
    (-> (promesa.core/let [k (bytes->aead-key (clj-ioutil.bytes/rand-bytes 32) a)]
          (reset! res k))
        (promesa.core/catch #(reset! res %))))
  (do
    (def k @res)
    (reset! res nil)
    (-> (promesa.core/let [d (aead-key->bytes k a)]
          (reset! res d))
        (promesa.core/catch #(reset! res %))))
  (do
    (reset! res nil)
    (def iv (clj-ioutil.bytes/rand-bytes 12))
    (def d (clj-ioutil.bytes/str->bytes "hello"))
    (-> (promesa.core/let [e (aead-encrypt d k iv a)]
          (reset! res e))
        (promesa.core/catch #(reset! res %))))
  (do
    (def e @res)
    (reset! res nil)
    (-> (promesa.core/let [d (aead-decrypt e k iv a)]
          (reset! res (clj-ioutil.bytes/bytes->str d)))
        (promesa.core/catch #(reset! res %)))))

;;; ec

(def ec-algos #{:p256 :p384 :p521})

(def ecdh-generate-keypair "[algo]" impl/ecdh-generate-keypair)
(def ecdh-pub->bytes "[pub]" impl/ecdh-pub->bytes)
(def ecdh-pri->bytes "[pri]" impl/ecdh-pri->bytes)
(def bytes->ecdh-pub "[data algo]" impl/bytes->ecdh-pub)
(def bytes->ecdh-pri "[data algo]" impl/bytes->ecdh-pri)
(def ecdh-key-size 48)
(def ecdh-key-exchange "[pub pri]" impl/ecdh-key-exchange)

(def ecdsa-generate-keypair "[algo]" impl/ecdsa-generate-keypair)
(def ecdsa-pub->bytes "[pub]" impl/ecdsa-pub->bytes)
(def ecdsa-pri->bytes "[pri]" impl/ecdsa-pri->bytes)
(def bytes->ecdsa-pub "[data algo]" impl/bytes->ecdsa-pub)
(def bytes->ecdsa-pri "[data algo]" impl/bytes->ecdsa-pri)
(def ecdsa-sign "[data pri algo]" impl/ecdsa-sign)
(def ecdsa-verify "[data sig pub algo]" impl/ecdsa-verify)

(def x25519-generate-keypair "[]" impl/x25519-generate-keypair)
(def x25519-pub->bytes "[pub]" impl/x25519-pub->bytes)
(def x25519-pri->bytes "[pri]" impl/x25519-pri->bytes)
(def bytes->x25519-pub "[data]" impl/bytes->x25519-pub)
(def bytes->x25519-pri "[data]" impl/bytes->x25519-pri)
(def x25519-key-size 32)
(def x25519-key-exchange "[pub pri]" impl/x25519-key-exchange)

(def x448-generate-keypair "[]" impl/x448-generate-keypair)
(def x448-pub->bytes "[pub]" impl/x448-pub->bytes)
(def x448-pri->bytes "[pri]" impl/x448-pri->bytes)
(def bytes->x448-pub "[data]" impl/bytes->x448-pub)
(def bytes->x448-pri "[data]" impl/bytes->x448-pri)
(def x448-key-size 56)
(def x448-key-exchange "[pub pri]" impl/x448-key-exchange)

(def ed25519-generate-keypair "[]" impl/ed25519-generate-keypair)
(def ed25519-pub->bytes "[pub]" impl/ed25519-pub->bytes)
(def ed25519-pri->bytes "[pri]" impl/ed25519-pri->bytes)
(def bytes->ed25519-pub "[data]" impl/bytes->ed25519-pub)
(def bytes->ed25519-pri "[data]" impl/bytes->ed25519-pri)
(def ed25519-sign "[data pri]" impl/ed25519-sign)
(def ed25519-verify "[data sig pub]" impl/ed25519-sign)

(def ed448-generate-keypair "[]" impl/ed448-generate-keypair)
(def ed448-pub->bytes "[pub]" impl/ed448-pub->bytes)
(def ed448-pri->bytes "[pri]" impl/ed448-pri->bytes)
(def bytes->ed448-pub "[data]" impl/bytes->ed448-pub)
(def bytes->ed448-pri "[data]" impl/bytes->ed448-pri)
(def ed448-sign "[data pri]" impl/ed448-sign)
(def ed448-verify "[data sig pub]" impl/ed448-verify)

(comment
  (do
    (def res (atom nil))
    (-> (promesa.core/let [kp (ecdh-generate-keypair :p384)]
          (reset! res kp))
        (promesa.core/catch #(reset! res %))))
  (do
    (def pri (first @res))
    (def pub (last @res)))
  ;; test pri fac
  (do
    (reset! res nil)
    (-> (promesa.core/let [d (ecdh-pri->bytes pri)]
          (reset! res d))
        (promesa.core/catch #(reset! res %))))
  (do
    (-> (promesa.core/let [k (bytes->ecdh-pri @res :p384)]
          (reset! res k))
        (promesa.core/catch #(reset! res %))))
  ;; test pub fac
  (do
    (reset! res nil)
    (-> (promesa.core/let [d (ecdh-pub->bytes pub)]
          (reset! res d))
        (promesa.core/catch #(reset! res %))))
  (do
    (-> (promesa.core/let [k (bytes->ecdh-pub @res :p384)]
          (reset! res k))
        (promesa.core/catch #(reset! res %))))
  ;; test ke
  (do
    (reset! res nil)
    (-> (promesa.core/let [k (ecdh-key-exchange pub pri)]
          (reset! res k))
        (promesa.core/catch #(reset! res nil)))))

(comment
  (do
    (def res (atom nil))
    (-> (promesa.core/let [kp (ecdsa-generate-keypair :p384)]
          (reset! res kp))
        (promesa.core/catch #(reset! res %))))
  (do
    (def pri (first @res))
    (def pub (last @res))
    (def d (clj-ioutil.bytes/str->bytes "hello")))
  ;; test sign
  (do
    (reset! res nil)
    (-> (promesa.core/let [s (ecdsa-sign d pri :sha384)]
          (reset! res s))
        (promesa.core/catch #(reset! res %))))
  (do
    (def s @res)
    (reset! res nil)
    (-> (promesa.core/let [ok (ecdsa-verify d s pub :sha384)]
          (reset! res ok))
        (promesa.core/catch #(reset! res %)))))

(comment
  (do
    (def res (atom nil))
    (-> (promesa.core/let [kp (x448-generate-keypair)]
          (reset! res kp))
        (promesa.core/catch #(reset! res %))))
  (do
    (def pri (first @res))
    (def pub (last @res)))
  ;; test pri fac
  (do
    (reset! res nil)
    (-> (promesa.core/let [d (x448-pri->bytes pri)]
          (reset! res d))
        (promesa.core/catch #(reset! res %))))
  (do
    (-> (promesa.core/let [k (bytes->x448-pri @res)]
          (reset! res k))
        (promesa.core/catch #(reset! res %))))
  ;; test pub fac
  (do
    (reset! res nil)
    (-> (promesa.core/let [d (x448-pub->bytes pub)]
          (reset! res d))
        (promesa.core/catch #(reset! res %))))
  (do
    (-> (promesa.core/let [k (bytes->x448-pub @res)]
          (reset! res k))
        (promesa.core/catch #(reset! res %))))
  ;; test ke
  (do
    (reset! res nil)
    (-> (promesa.core/let [k (x448-key-exchange pub pri)]
          (reset! res k))
        (promesa.core/catch #(reset! res nil)))))

(comment
  (do
    (def res (atom nil))
    (-> (promesa.core/let [kp (ed448-generate-keypair)]
          (reset! res kp))
        (promesa.core/catch #(reset! res %))))
  (do
    (def pri (first @res))
    (def pub (last @res))
    (def d (clj-ioutil.bytes/str->bytes "hello")))
  ;; test sign
  (do
    (reset! res nil)
    (-> (promesa.core/let [s (ed448-sign d pri)]
          (reset! res s))
        (promesa.core/catch #(reset! res %))))
  (do
    (def s @res)
    (reset! res nil)
    (-> (promesa.core/let [ok (ed448-verify d s pub)]
          (reset! res ok))
        (promesa.core/catch #(reset! res %)))))
