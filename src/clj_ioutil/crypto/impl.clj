(ns clj-ioutil.crypto.impl
  (:require [promesa.core :as p]
            [clj-ioutil.bytes :as b])
  (:import java.nio.ByteBuffer
           [java.security
            Key PublicKey PrivateKey KeyPair KeyPairGenerator KeyFactory
            MessageDigest Signature]
           [java.security.spec
            EncodedKeySpec X509EncodedKeySpec PKCS8EncodedKeySpec
            AlgorithmParameterSpec ECGenParameterSpec]
           [javax.crypto
            SecretKey KeyGenerator KeyAgreement Mac Cipher]
           [javax.crypto.spec
            SecretKeySpec
            IvParameterSpec GCMParameterSpec]))

;;; digest

(def digest-algo
  {:sha256 "SHA-256"
   :sha384 "SHA-384"
   :sha512 "SHA-512"})

(def hmac-algo
  {:sha256 "HmacSHA256"
   :sha384 "HmacSHA384"
   :sha512 "HmacSHA512"})

(defn digest [^bytes data algo]
  (p/vthread
   (let [^MessageDigest md (MessageDigest/getInstance (digest-algo algo))]
     (.digest md data))))

(defn hmac [^bytes data key algo]
  (p/vthread
   (let [^Mac m (Mac/getInstance (hmac-algo algo))
         ^SecretKey key (if-not (bytes? key) key (SecretKeySpec. key "Hmac"))]
     (.init m key)
     (.doFinal m data))))

;;; cipher

(def cipher-mode
  {:encrypt Cipher/ENCRYPT_MODE
   :decrypt Cipher/DECRYPT_MODE
   :wrap    Cipher/WRAP_MODE
   :unwrap  Cipher/UNWRAP_MODE})

(def cipher-key-algo
  {:aes128-cbc        "AES"
   :aes192-cbc        "AES"
   :aes256-cbc        "AES"
   :aes128-ctr        "AES"
   :aes192-ctr        "AES"
   :aes256-ctr        "AES"
   :aes128-gcm        "AES"
   :aes192-gcm        "AES"
   :aes256-gcm        "AES"
   :chacha20-poly1305 "ChaCha20"})

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

(def cipher-algo
  {:aes128-cbc        "AES/CBC/PKCS5Padding"
   :aes192-cbc        "AES/CBC/PKCS5Padding"
   :aes256-cbc        "AES/CBC/PKCS5Padding"
   :aes128-ctr        "AES/CTR/NoPadding"
   :aes192-ctr        "AES/CTR/NoPadding"
   :aes256-ctr        "AES/CTR/NoPadding"
   :aes128-gcm        "AES/GCM/NoPadding"
   :aes192-gcm        "AES/GCM/NoPadding"
   :aes256-gcm        "AES/GCM/NoPadding"
   :chacha20-poly1305 "ChaCha20-Poly1305"})

(defn cipher-params [algo iv]
  (case algo
    (:aes128-gcm :aes192-gcm :aes256-gcm)
    (GCMParameterSpec. 128 iv)
    (:aes128-cbc :aes192-cbc :aes256-cbc :aes128-ctr :aes192-ctr :aes256-ctr :chacha20-poly1305)
    (IvParameterSpec. iv)))

(defn generate-key [algo]
  (p/vthread
   (let [^KeyGenerator kg (KeyGenerator/getInstance (cipher-key-algo algo))]
     (.init kg (bit-shift-left (cipher-key-size algo) 3))
     (.generateKey kg))))

(defn bytes->key [^bytes data algo]
  (p/vthread
   (assert (= (b/blength data) (cipher-key-size algo)))
   (SecretKeySpec. data (cipher-key-algo algo))))

(defn key->bytes [^SecretKey key algo]
  (p/vthread
   (let [^bytes data (.getEncoded key)]
     (assert (= (b/blength data) (cipher-key-size algo)))
     data)))

(defn crypt [mode ^bytes data key ^bytes iv algo & {:keys [aad]}]
  (p/vthread
   (let [^Cipher c (Cipher/getInstance (cipher-algo algo))
         ^SecretKey key (if-not (bytes? key) key (SecretKeySpec. key (cipher-key-algo algo)))]
     (.init c ^int (cipher-mode mode) key ^AlgorithmParameterSpec (cipher-params algo iv))
     (when aad
       (.updateAAD c aad))
     (.doFinal c data))))

(def encrypt (partial crypt :encrypt))
(def decrypt (partial crypt :decrypt))

;;; pk

(def kp-algo
  {:ecdh-p256         "EC"
   :ecdh-p384         "EC"
   :ecdh-p521         "EC"
   :ecdsa-p256-sha256 "EC"
   :ecdsa-p256-sha384 "EC"
   :ecdsa-p256-sha512 "EC"
   :ecdsa-p384-sha256 "EC"
   :ecdsa-p384-sha384 "EC"
   :ecdsa-p384-sha512 "EC"
   :ecdsa-p521-sha256 "EC"
   :ecdsa-p521-sha384 "EC"
   :ecdsa-p521-sha512 "EC"
   :x25519            "X25519"
   :x448              "X448"
   :ed25519           "Ed25519"
   :ed448             "Ed448"})

(def secp256r1 (ECGenParameterSpec. "SECP256R1"))
(def secp384r1 (ECGenParameterSpec. "SECP384R1"))
(def secp521r1 (ECGenParameterSpec. "SECP521R1"))

(def kp-params
  {:ecdh-p256         secp256r1
   :ecdh-p384         secp384r1
   :ecdh-p521         secp521r1
   :ecdsa-p256-sha256 secp256r1
   :ecdsa-p256-sha384 secp256r1
   :ecdsa-p256-sha512 secp256r1
   :ecdsa-p384-sha256 secp384r1
   :ecdsa-p384-sha384 secp384r1
   :ecdsa-p384-sha512 secp384r1
   :ecdsa-p521-sha256 secp521r1
   :ecdsa-p521-sha384 secp521r1
   :ecdsa-p521-sha512 secp521r1})

(def key-spec
  {:x509  X509EncodedKeySpec
   :pkcs8 PKCS8EncodedKeySpec})

(defn new-key-spec [data format]
  (case format
    :x509  (X509EncodedKeySpec. data)
    :pkcs8 (PKCS8EncodedKeySpec. data)))

(def pri-format
  {:ecdh-p256         :pkcs8
   :ecdh-p384         :pkcs8
   :ecdh-p521         :pkcs8
   :ecdsa-p256-sha256 :pkcs8
   :ecdsa-p256-sha384 :pkcs8
   :ecdsa-p256-sha512 :pkcs8
   :ecdsa-p384-sha256 :pkcs8
   :ecdsa-p384-sha384 :pkcs8
   :ecdsa-p384-sha512 :pkcs8
   :ecdsa-p521-sha256 :pkcs8
   :ecdsa-p521-sha384 :pkcs8
   :ecdsa-p521-sha512 :pkcs8
   :x25519            :pkcs8
   :x448              :pkcs8
   :ed25519           :pkcs8
   :ed448             :pkcs8})

(def pub-format
  {:ecdh-p256         :x509
   :ecdh-p384         :x509
   :ecdh-p521         :x509
   :ecdsa-p256-sha256 :x509
   :ecdsa-p256-sha384 :x509
   :ecdsa-p256-sha512 :x509
   :ecdsa-p384-sha256 :x509
   :ecdsa-p384-sha384 :x509
   :ecdsa-p384-sha512 :x509
   :ecdsa-p521-sha256 :x509
   :ecdsa-p521-sha384 :x509
   :ecdsa-p521-sha512 :x509
   :x25519            :x509
   :x448              :x509
   :ed25519           :x509
   :ed448             :x509})

(def ke-algo
  {:ecdh-p256 "ECDH"
   :ecdh-p384 "ECDH"
   :ecdh-p521 "ECDH"
   :x25519    "X25519"
   :x448      "X448"})

(def sign-algo
  {:ecdsa-p256-sha256 "SHA256withECDSA"
   :ecdsa-p256-sha384 "SHA384withECDSA"
   :ecdsa-p256-sha512 "SHA512withECDSA"
   :ecdsa-p384-sha256 "SHA256withECDSA"
   :ecdsa-p384-sha384 "SHA384withECDSA"
   :ecdsa-p384-sha512 "SHA512withECDSA"
   :ecdsa-p521-sha256 "SHA256withECDSA"
   :ecdsa-p521-sha384 "SHA384withECDSA"
   :ecdsa-p521-sha512 "SHA512withECDSA"
   :ed25519           "Ed25519"
   :ed448             "Ed448"})

(defn generate-keypair [algo]
  (p/vthread
   (let [^KeyPairGenerator kpg (KeyPairGenerator/getInstance (kp-algo algo))]
     (when-let [params (kp-params algo)]
       (if-not (instance? AlgorithmParameterSpec params)
         (.initialize kpg ^int params)
         (.initialize kpg ^AlgorithmParameterSpec params)))
     (let [^KeyPair kp (.generateKeyPair kpg)
           ^PrivateKey pri (.getPrivate kp)
           ^PublicKey pub (.getPublic kp)]
       [pri pub]))))

(defn bytes->pri [^bytes data algo]
  (p/vthread
   (let [^KeyFactory kf (KeyFactory/getInstance (kp-algo algo))
         ^EncodedKeySpec ks (new-key-spec data (pri-format algo))]
     (.generatePrivate kf ks))))

(defn bytes->pub [^bytes data algo]
  (p/vthread
   (let [^KeyFactory kf (KeyFactory/getInstance (kp-algo algo))
         ^EncodedKeySpec ks (new-key-spec data (pub-format algo))]
     (.generatePublic kf ks))))

(defn pri->bytes [^PrivateKey key algo]
  (p/vthread
   (let [^KeyFactory kf (KeyFactory/getInstance (kp-algo algo))
         ^EncodedKeySpec ks (.getKeySpec kf key (key-spec (pri-format algo)))]
     (.getEncoded ks))))

(defn pub->bytes [^PublicKey key algo]
  (p/vthread
   (let [^KeyFactory kf (KeyFactory/getInstance (kp-algo algo))
         ^EncodedKeySpec ks (.getKeySpec kf key (key-spec (pub-format algo)))]
     (.getEncoded ks))))

(defn key-exchange [^PublicKey pub ^PrivateKey pri algo & {:keys [size]}]
  (p/vthread
   (let [^KeyAgreement ka (KeyAgreement/getInstance (ke-algo algo))]
     (.init ka pri)
     (.doPhase ka pub true)
     (let [data (.generateSecret ka)]
       (if-not size
         data
         (do
           (assert (>= (b/blength data) size))
           (b/sub data 0 size)))))))

(defn sign [^bytes data ^PrivateKey pri algo]
  (p/vthread
   (let [^Signature s (Signature/getInstance (sign-algo algo))]
     (.initSign s pri)
     (.update s data)
     (.sign s))))

(defn verify [^bytes data ^bytes sig ^PublicKey pub algo]
  (p/vthread
   (let [^Signature s (Signature/getInstance (sign-algo algo))]
     (.initVerify s pub)
     (.update s data)
     (.verify s sig))))
