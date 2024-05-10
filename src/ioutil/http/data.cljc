(ns http.data
  (:require [clojure.string :as str]
            [clojure.edn :as edn]
            [ioutil.bytes :as b]))

;;; x-www-form-urlencoded

(defn- str->form-urlencoded [s]
  (str/replace (b/str->urlencoded s) "%20" "+"))

(defn- form-urlencoded->str [u]
  (b/urlencoded->str (str/replace u "+" "%20")))

(defn clj->form [x]
  (assert (map? x))
  (->> x
       (map
        (fn [[k v]]
          (str (str->form-urlencoded (if (keyword? k) (name k) k))
               \=
               (str->form-urlencoded (str v)))))
       (str/join \&)))

(defn form->clj [x & {:keys [keywordize-keys] :or {keywordize-keys false}}]
  (->> (str/split x #"&")
       (map
        #(let [[k v] (str/split % #"=" 2)]
           (assert v)
           (let [k (form-urlencoded->str k)
                 v (form-urlencoded->str v)
                 k (if-not keywordize-keys k (keyword k))]
             [k v])))
       (into {})))

;;; json
;; TODO

;;; edn

(defn clj->edn [x]
  (pr-str x))

(defn edn->clj [x]
  (edn/read-string x))
