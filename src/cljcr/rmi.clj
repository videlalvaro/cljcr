(ns cljcr.rmi
  (:use [clojure.contrib.except :only (throwf)])
  (:import (org.apache.jackrabbit.rmi.repository URLRemoteRepository)
           (javax.jcr SimpleCredentials)
           (javax.jcr.query Query)))

(defn get-credentials [username password]
  (SimpleCredentials. username (.toCharArray password)))

;; TODO implicitly add /rmi to the url
(defn get-repository [url]
  (URLRemoteRepository. url))

(defn get-session
  ([repo creds]
     (.login repo creds))
  ([repo creds workspace]
     (.login repo creds workspace)))

(defn get-workspace [session]
  (.getWorkspace session))

(defn get-query-manager [workspace]
  (.getQueryManager workspace))

(defn create-query [qm statement language]
  (.createQuery qm statement language))

(defn execute [query]
  (.execute query))

(defn get-nodes [result]
  (iterator-seq (.getNodes result)))

(defn get-query-type [type]
  (if (= type :sql)
    Query/JCR_SQL2
    Query/XPATH))

(defn get-test-qm []
  (let [url "http://localhost:8080/rmi"
        wk-name "nzz"
        credentials (SimpleCredentials. "admin" (.toCharArray "admin"))]
    (.. (cljcr.rmi/get-repository "http://localhost:8080/rmi")
        (login credentials "nzz")
        (getWorkspace)
        (getQueryManager))))