(ns clj-ioutil.clj.crypto
  (:require [clj-ioutil.bytes :as b])
  (:import java.nio.ByteBuffer
           [java.security
            MessageDigest Key PublicKey PrivateKey KeyPair KeyPairGenerator KeyFactory Signature]
           [java.security.spec
            EncodedKeySpec X509EncodedKeySpec PKCS8EncodedKeySpec
            AlgorithmParameterSpec ECGenParameterSpec RSAKeyGenParameterSpec MGF1ParameterSpec PSSParameterSpec]
           [javax.crypto
            SecretKey KeyGenerator KeyAgreement Mac Cipher]
           [javax.crypto.spec
            IvParameterSpec GCMParameterSpec ChaCha20ParameterSpec]))

;;; params

(defmulti make-algo-params (fn [type opts] type))

(defn algo-params [type & {:as opts}]
  (make-algo-params type opts))

;;;; ec

;; std-name: X25519 ED25519 P-256 SECP256R1
(defmethod make-algo-params :ec-gen [type {:keys [std-name]}]
  (ECGenParameterSpec. std-name))

;;;; rsa

(defmethod make-algo-params :rsa-key-gen
  [type {:keys [keysize public-exponent key-params]
         :or {keysize 4096 public-exponent RSAKeyGenParameterSpec/F4}}]
  (if-not key-params
    (RSAKeyGenParameterSpec. keysize public-exponent)
    (RSAKeyGenParameterSpec. keysize public-exponent ^AlgorithmParameterSpec key-params)))

;; md-name: SHA1 SHA224 SHA256 SHA384 SHA512 SHA512-224 SHA512-256
(defmethod make-algo-params :mgf1 [type {:keys [md-name]}]
  (MGF1ParameterSpec. md-name))

;; salt-len: Math.ceil((keySizeInBits - 1) / 8) - digestSizeInBytes - 2;
(defmethod make-algo-params :pss [type {:keys [salt-len] :or {salt-len 32}}]
  (PSSParameterSpec. salt-len))

;;;; cipher

;; iv: bytes
(defmethod make-algo-params :iv [type {:keys [iv]}]
  (IvParameterSpec. iv))

;; iv: 12 bytes
;; tag-length: 32, 64, 96, ..., 128
(defmethod make-algo-params :gcm
  [type {:keys [tlen iv] :or {tlen 128}}]
  (GCMParameterSpec. tlen (or iv (b/rand-bytes 12))))

(defmethod make-algo-params :chacha20
  [type {:keys [nonce counter] :or {counter 0}}]
  (ChaCha20ParameterSpec. (or nonce (b/rand-bytes 12)) counter))

;;; security

;; algo: MD5 SHA-1 SHA-256
(defn digest [^bytes data algo]
  (let [^MessageDigest m (MessageDigest/getInstance algo)]
    (.digest m data)))

;; algo: DH DSA RSA
(defn create-keypair [algo & {:keys [params]}]
  (let [^KeyPairGenerator g (KeyPairGenerator/getInstance algo)]
    (when params
      (if-not (instance? params AlgorithmParameterSpec)
        (.initialize g ^int params)
        (.initialize g ^AlgorithmParameterSpec params)))
    (let [^KeyPair kp (.generateKeyPair g)]
      [(.getPrivate kp) (.getPublic kp)])))

(defn key-algo [^Key key]
  (.getAlgorithm key))

(def key-spec
  {:x509  X509EncodedKeySpec
   :pkcs8 PKCS8EncodedKeySpec})

(defn ^EncodedKeySpec new-key-spec [data spec]
  (case spec
    :x509  (X509EncodedKeySpec. data)
    :pkcs8 (PKCS8EncodedKeySpec. data)))

(defn key->data [^Key key & {:keys [spec] :or {spec :x509}}]
  (let [^KeyFactory f (KeyFactory/getInstance (key-algo key))]
    (.getEncoded ^EncodedKeySpec (.getKeySpec f key (key-spec spec)))))

(defn ^PublicKey data->pub [^bytes data algo & {:keys [spec] :or {spec :x509}}]
  (let [^KeyFactory f (KeyFactory/getInstance algo)]
    (.generatePublic f (new-key-spec data spec))))

(defn ^PrivateKey data->pri [^bytes data algo & {:keys [spec] :or {spec :x509}}]
  (let [^KeyFactory f (KeyFactory/getInstance algo)]
    (.generatePrivate f (new-key-spec data spec))))

(defn sign [^bytes data ^PrivateKey key & {:keys [params]}]
  (let [^Signature s (Signature/getInstance (key-algo key))]
    (when params
      (.setParameter s ^AlgorithmParameterSpec params))
    (.initSign s key)
    (.update s data)
    (.sign s)))

(defn verify [^bytes data ^bytes sig ^PublicKey key {:keys [params]}]
  (let [^Signature s (Signature/getInstance (key-algo key))]
    (when params
      (.setParameter s ^AlgorithmParameterSpec params))
    (.initVerify s key)
    (.update s data)
    (.verify s sig)))

(defn key-ex [^PublicKey pub ^PrivateKey pri & {:keys [params algo]}]
  (let [^KeyAgreement ka (KeyAgreement/getInstance (key-algo pri))]
    (if-not params
      (.init ka pri)
      (.init ka pri ^AlgorithmParameterSpec params))
    (.doPhase ka pub true)
    (if-not algo
      (.generateSecret ka)
      (.generateSecret ka algo))))

;;; crypto

;; algo: AES DESede HmacSHA1 HmacSHA256
(defn create-key [algo & {:keys [params]}]
  (let [^KeyGenerator g (KeyGenerator/getInstance algo)]
    (when params
      (if-not (instance? params AlgorithmParameterSpec)
        (.init g ^int params)
        (.init g ^AlgorithmParameterSpec params)))
    (.generateKey g)))

;; algo: HmacSHA1 HmacSHA256
(defn mac [^bytes data ^Key key algo & {:keys [params]}]
  (let [^Mac m (Mac/getInstance algo)]
    (if-not params
      (.init m key)
      (.init m key ^AlgorithmParameterSpec params))
    (.doFinal m data)))

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
