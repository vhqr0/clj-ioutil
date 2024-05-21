(ns clj-ioutil.crypto.impl
  (:require [promesa.core :as p]
            [clj-ioutil.bytes :as b])
  (:import java.nio.ByteBuffer
           [java.security
            MessageDigest Key PublicKey PrivateKey KeyPair KeyPairGenerator KeyFactory Signature]
           [java.security.spec
            EncodedKeySpec X509EncodedKeySpec PKCS8EncodedKeySpec
            AlgorithmParameterSpec ECGenParameterSpec RSAKeyGenParameterSpec MGF1ParameterSpec PSSParameterSpec]
           [javax.crypto
            SecretKey KeyGenerator KeyAgreement Mac Cipher]
           [javax.crypto.spec
            SecretKeySpec
            IvParameterSpec GCMParameterSpec ChaCha20ParameterSpec
            OAEPParameterSpec PSource$PSpecified]))

;;; params

(defmulti make-algo-params (fn [type opts] type))

(defn algo-params [type & {:as opts}]
  (make-algo-params type opts))

;;;; cipher

;; iv: bytes
(defmethod make-algo-params :iv [type {:keys [iv]}]
  (IvParameterSpec. iv))

;; iv: 12 bytes
;; tlen: 32, 64, 96, ..., 128
(defmethod make-algo-params :gcm
  [type {:keys [tlen iv] :or {tlen 128}}]
  (GCMParameterSpec. tlen (or iv (b/rand-bytes 12))))

;; ChaCha20 Counter: This can be set to any number, but will usually
;; be zero or one. It makes sense to use one if we use the zero block
;; for something else, such as generating a one-time authenticator key
;; as part of an AEAD algorithm.
;;
;; [https://datatracker.ietf.org/doc/html/rfc7539]

;; nonce: 12 bytes
(defmethod make-algo-params :chacha20
  [type {:keys [nonce counter] :or {counter 1}}]
  (ChaCha20ParameterSpec. (or nonce (b/rand-bytes 12)) counter))

;;;; ec

;; std-name: SECP256R1 SECP384R1 SECP521R1
(defmethod make-algo-params :ec-gen [type {:keys [std-name]}]
  (ECGenParameterSpec. std-name))

;;;; rsa

(defmethod make-algo-params :rsa-key-gen
  [type {:keys [keysize public-exponent key-params]
         :or {keysize 4096 public-exponent RSAKeyGenParameterSpec/F4}}]
  (if-not key-params
    (RSAKeyGenParameterSpec. keysize public-exponent)
    (RSAKeyGenParameterSpec. keysize public-exponent ^AlgorithmParameterSpec key-params)))

;; md-name: SHA256 SHA384 SHA512 SHA512-224 SHA512-256
(defmethod make-algo-params :mgf1 [type {:keys [md-name]}]
  (MGF1ParameterSpec. md-name))

;; salt-len: Math.ceil((keySizeInBits - 1) / 8) - digestSizeInBytes - 2;
(defmethod make-algo-params :pss
  [type {:keys [md-name mgf-name mgf-params salt-len trailer-field]
         :or {mgf-name "MGF1" trailer-field PSSParameterSpec/TRAILER_FIELD_BC}}]
  (let [mgf-params (or mgf-params (algo-params :mgf1 :md-name md-name))]
    (PSSParameterSpec. md-name mgf-name mgf-params salt-len trailer-field)))

(defmethod make-algo-params :oaep
  [type {:keys [md-name mgf-name mgf-params p-src]
         :or {mgf-name "MGF1" p-src PSource$PSpecified/DEFAULT}}]
  (let [mgf-params (or mgf-params (algo-params :mgf1 :md-name md-name))]
    (OAEPParameterSpec. md-name mgf-name mgf-params p-src)))

;;; key

;; algo: HmacSHA256 HmacSHA384 HmacSHA512 AES ChaCha20
(defn ^SecretKey generate-key [algo & {:keys [params]}]
  (let [^KeyGenerator g (KeyGenerator/getInstance algo)]
    (when params
      (if-not (instance? AlgorithmParameterSpec params)
        (.init g ^int params)
        (.init g ^AlgorithmParameterSpec params)))
    (.generateKey g)))

;; algo: RSA EC X25519 X448 Ed25519 Ed448
(defn generate-keypair [algo & {:keys [params]}]
  (let [^KeyPairGenerator g (KeyPairGenerator/getInstance algo)]
    (when params
      (if-not (instance? AlgorithmParameterSpec params)
        (.initialize g ^int params)
        (.initialize g ^AlgorithmParameterSpec params)))
    (let [^KeyPair kp (.generateKeyPair g)]
      [(.getPrivate kp) (.getPublic kp)])))

(def key-format
  {:x509  X509EncodedKeySpec
   :pkcs8 PKCS8EncodedKeySpec})

(defn ^EncodedKeySpec new-key-format [data format]
  (case format
    :x509  (X509EncodedKeySpec. data)
    :pkcs8 (PKCS8EncodedKeySpec. data)))

(defn key->data [^Key key algo format]
  (let [^KeyFactory f (KeyFactory/getInstance algo)]
    (.getEncoded ^EncodedKeySpec (.getKeySpec f key (key-format format)))))

(defn ^PublicKey data->pub [^bytes data algo format]
  (let [^KeyFactory f (KeyFactory/getInstance algo)]
    (.generatePublic f (new-key-format data format))))

(defn ^PrivateKey data->pri [^bytes data algo format]
  (let [^KeyFactory f (KeyFactory/getInstance algo)]
    (.generatePrivate f (new-key-format data format))))

;;; digest

;; algo: SHA-256 SHA-384 SHA-512
(defn digest [^bytes data algo]
  (let [^MessageDigest m (MessageDigest/getInstance algo)]
    (.digest m data)))

;; algo: HmacSHA256 HmacSHA384 HmacSHA512
(defn hmac [^bytes data ^Key key algo & {:keys [params]}]
  (let [^Mac m (Mac/getInstance algo)]
    (if-not params
      (.init m key)
      (.init m key ^AlgorithmParameterSpec params))
    (.doFinal m data)))

;;; cipher

(def cipher-mode
  {:encrypt Cipher/ENCRYPT_MODE
   :decrypt Cipher/DECRYPT_MODE
   :wrap    Cipher/WRAP_MODE
   :unwrap  Cipher/UNWRAP_MODE})

(defn crypt [mode ^bytes data ^Key key algo & {:keys [aad params]}]
  (let [^Cipher c (Cipher/getInstance algo)]
    (if-not params
      (.init c ^int (cipher-mode mode) key)
      (.init c ^int (cipher-mode mode) key ^AlgorithmParameterSpec params))
    (when aad
      (if-not (instance? ByteBuffer aad)
        (.updateAAD c ^bytes aad)
        (.updateAAD c ^ByteBuffer aad)))
    (.doFinal c data)))

(def encrypt (partial crypt :encrypt))
(def decrypt (partial crypt :decrypt))
(def wrap-key (partial crypt :wrap))
(def unwrap-key (partial crypt :unwrap))

;;; ke/sign

;; algo: ECDH X25519 X448
(defn key-exchange [^PublicKey pub ^PrivateKey pri algo & {:keys [params]}]
  (let [^KeyAgreement ka (KeyAgreement/getInstance algo)]
    (if-not params
      (.init ka pri)
      (.init ka pri ^AlgorithmParameterSpec params))
    (.doPhase ka pub true)
    (.generateSecret ka)))

;; algo: SHA256withECDSA SHA384withECDSA SHA512withECDSA Ed25519 Ed448
(defn sign [^bytes data ^PrivateKey pri algo & {:keys [params]}]
  (let [^Signature s (Signature/getInstance algo)]
    (when params
      (.setParameter s ^AlgorithmParameterSpec params))
    (.initSign s pri)
    (.update s data)
    (.sign s)))

(defn verify [^bytes data ^bytes sig ^PublicKey pub algo & {:keys [params]}]
  (let [^Signature s (Signature/getInstance algo)]
    (when params
      (.setParameter s ^AlgorithmParameterSpec params))
    (.initVerify s pub)
    (.update s data)
    (.verify s sig)))

;;; impl

(def digest-algo
  {:sha256 "SHA-256"
   :sha384 "SHA-384"
   :sha512 "SHA-512"})

(def hmac-algo
  {:sha256 "HmacSHA256"
   :sha384 "HmacSHA384"
   :sha512 "HmacSHA512"})

(defn bytes->digest [data algo] (p/vthread (digest data (digest-algo algo))))
(defn bytes->hmac [data key algo] (p/vthread (hmac data (if-not (bytes? key) key (SecretKeySpec. key "Hmac")) (hmac-algo algo))))

(defn aead-key->bytes [^SecretKeySpec key algo]
  (p/vthread
   (.getEncoded key)))

(defn bytes->aead-key [data algo]
  (p/vthread
   (case algo
     :aes128gcm        (do (assert (b/blength data) 16) (SecretKeySpec. data "AES"))
     :aes256gcm        (do (assert (b/blength data) 32) (SecretKeySpec. data "AES"))
     :chacha20poly1305 (do (assert (b/blength data) 32) (SecretKeySpec. data "ChaCha20")))))

(defn- aead-crypt [mode data key iv algo & {:keys [aad]}]
  (p/let [key (if-not (bytes? key) key (bytes->aead-key data algo))]
    (case algo
      :aes128gcm        (crypt mode data key "AES/GCM/NoPadding" :aad aad :params (algo-params :gcm :iv iv))
      :aes256gcm        (crypt mode data key "AES/GCM/NoPadding" :aad aad :params (algo-params :gcm :iv iv))
      :chacha20poly1305 (crypt mode data key "Chacha20-Poly1305" :aad aad :params (algo-params :iv :iv iv)))))

(def aead-encrypt (partial aead-crypt :encrypt))
(def aead-decrypt (partial aead-crypt :decrypt))

(def ec-algo
  {:p256 "SECP256R1"
   :p384 "SECP384R1"
   :p521 "SECP521R1"})

(defn ec-generate-keypair [algo]
  (p/vthread
   (generate-keypair "EC" :params (algo-params :ec-gen :std-name (ec-algo algo)))))

(defn ec-pub->bytes [pub] (p/vthread (key->data pub "EC" :x509)))
(defn ec-pri->bytes [pri] (p/vthread (key->data pri "EC" :pkcs8)))
(defn bytes->ec-pub [data algo] (p/vthread (data->pub data "EC" :x509)))
(defn bytes->ec-pri [data algo] (p/vthread (data->pri data "EC" :pkcs8)))

(def ecdh-generate-keypair ec-generate-keypair)
(def ecdh-pub->bytes ec-pub->bytes)
(def ecdh-pri->bytes ec-pri->bytes)
(def bytes->ecdh-pub bytes->ec-pub)
(def bytes->ecdh-pri bytes->ec-pri)

(defn ecdh-key-exchange [pub pri] (p/vthread (key-exchange pub pri "ECDH")))

(def ecdsa-generate-keypair ec-generate-keypair)
(def ecdsa-pub->bytes ec-pub->bytes)
(def ecdsa-pri->bytes ec-pri->bytes)
(def bytes->ecdsa-pub bytes->ec-pub)
(def bytes->ecdsa-pri bytes->ec-pri)

(def ecdsa-algo
  {:sha256 "SHA256withECDSA"
   :sha384 "SHA384withECDSA"
   :sha512 "SHA512withECDSA"})

(defn ecdsa-sign [data pri algo] (p/vthread (sign data pri (ecdsa-algo algo))))
(defn ecdsa-verify [data sig pub algo] (p/vthread (verify data sig pub (ecdsa-algo algo))))

(defn x25519-generate-keypair [] (p/vthread (generate-keypair "X25519")))
(defn x25519-pub->bytes [pub] (p/vthread (key->data pub "X25519" :x509)))
(defn x25519-pri->bytes [pri] (p/vthread (key->data pri "X25519" :pkcs8)))
(defn bytes->x25519-pub [data] (p/vthread (data->pub data "X25519" :x509)))
(defn bytes->x25519-pri [data] (p/vthread (data->pri data "X25519" :pkcs8)))
(defn x25519-key-exchange [pub pri] (p/vthread (key-exchange pub pri "X25519")))

(defn x448-generate-keypair [] (p/vthread (generate-keypair "X448")))
(defn x448-pub->bytes [pub] (p/vthread (key->data pub "X448" :x509)))
(defn x448-pri->bytes [pri] (p/vthread (key->data pri "X448" :pkcs8)))
(defn bytes->x448-pub [data] (p/vthread (data->pub data "X448" :x509)))
(defn bytes->x448-pri [data] (p/vthread (data->pri data "X448" :pkcs8)))
(defn x448-key-exchange [pub pri] (p/vthread (key-exchange pub pri "X448")))

(defn ed25519-generate-keypair [] (p/vthread (generate-keypair "Ed25519")))
(defn ed25519-pub->bytes [pub] (p/vthread (key->data pub "Ed25519" :x509)))
(defn ed25519-pri->bytes [pri] (p/vthread (key->data pri "Ed25519" :pkcs8)))
(defn bytes->ed25519-pub [data] (p/vthread (data->pub data "Ed25519" :x509)))
(defn bytes->ed25519-pri [data] (p/vthread (data->pri data "Ed25519" :pkcs8)))
(defn ed25519-sign [data pri] (p/vthread (sign data pri "Ed25519")))
(defn ed25519-verify [data sig pub] (p/vthread (verify data sig pub "Ed25519")))

(defn ed448-generate-keypair [] (p/vthread (generate-keypair "Ed448")))
(defn ed448-pub->bytes [pub] (p/vthread (key->data pub "Ed448" :x509)))
(defn ed448-pri->bytes [pri] (p/vthread (key->data pri "Ed448" :pkcs8)))
(defn bytes->ed448-pub [data] (p/vthread (data->pub data "Ed448" :x509)))
(defn bytes->ed448-pri [data] (p/vthread (data->pri data "Ed448" :pkcs8)))
(defn ed448-sign [data pri] (p/vthread (sign data pri "Ed448")))
(defn ed448-verify [data sig pub] (p/vthread (verify data sig pub "Ed448")))
