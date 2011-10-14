(ns 
  clj-http.core-test
  (:use 
    [clojure.test]
    [clj-http.run-server])
  (:require 
    [clojure.contrib.pprint :as pp]
    [clojure.contrib.io :as io]
    [clj-http.core :as core]
    [clj-http.util :as util]
    [clj-http.run-server :as rs])
  (:import 
    (org.apache.http HttpHost)
    (org.apache.http.impl.client DefaultHttpClient)
    (org.apache.http.client.params CookiePolicy ClientPNames)
    (org.apache.http.impl.conn.tsccm ThreadSafeClientConnManager)
    (org.apache.http.params BasicHttpParams)
    (org.apache.http.conn.params ConnRoutePNames)
    (org.apache.http.conn.scheme SchemeRegistry)))

(use-fixtures :once rs/before-once)

(deftest basic-http-client-test
  (let [c (core/basic-http-client)]
    (is (= (doto (.getParams (DefaultHttpClient.))
             (.setParameter ClientPNames/COOKIE_POLICY
                            CookiePolicy/BROWSER_COMPATIBILITY)))
        (.getParams (core/basic-http-client)))))

(deftest pooled-default
  (let [pc (core/pooled-http-client)]
    (is (instance? ThreadSafeClientConnManager (.getConnectionManager pc))))) 

(deftest pooled-with-args
  (let [pc (core/pooled-http-client 
           {:ttl 1 :max-total-conns 2 :max-per-route 3 :use-proxy true})
        cmgr (. pc getConnectionManager)
        http-params ^BasicHttpParams (. pc getParams)
        proxy-host ^HttpHost (. http-params getParameter ConnRoutePNames/DEFAULT_PROXY)]
    (is (instance? ThreadSafeClientConnManager cmgr))
    (is (instance? SchemeRegistry (. cmgr getSchemeRegistry)))
    (is (= 2 (. cmgr getMaxTotal)))
    (is (= 3 (. cmgr getDefaultMaxPerRoute))) 
    (is (= "localhost" (. proxy-host getHostName)))
    (is (= 8888 (. proxy-host getPort)))))

; From this point jetty listening on localhost:8080 is required
(def base-req
  {:scheme "http"
   :server-name "localhost"
   :server-port 8080})

(defn request [req]
  (core/request (merge base-req req)))

(defn slurp-body [req]
  (io/slurp* (:body req)))

(deftest makes-get-request
  (let [resp (request {:request-method :get :uri "/get"})]
    (is (= 200 (:status resp)))
    (is (= "get" (slurp-body resp)))))

(deftest makes-head-request
  (let [resp (request {:request-method :head :uri "/head"})]
    (is (= 200 (:status resp)))
    (is (nil? (:body resp)))))

(deftest sets-content-type-with-charset
  (let [resp (request {:request-method :get :uri "/content-type"
                       :content-type "text/plain" :character-encoding "UTF-8"})]
    (is (= "text/plain; charset=UTF-8" (slurp-body resp)))))

(deftest sets-content-type-without-charset
  (let [resp (request {:request-method :get :uri "/content-type"
                       :content-type "text/plain"})]
    (is (= "text/plain" (slurp-body resp)))))

(deftest sets-arbitrary-headers
  (let [resp (request {:request-method :get :uri "/header"
                       :headers {"X-My-Header" "header-val"}})]
    (is (= "header-val" (slurp-body resp)))))

(deftest sends-and-returns-byte-array-body
  (let [resp (request {:request-method :post :uri "/post"
                       :body (util/utf8-bytes "contents")})]
    (is (= 200 (:status resp)))
    (is (= "contents" (slurp-body resp)))))

(deftest returns-arbitrary-headers
  (let [resp (request {:request-method :get :uri "/get"})]
    (is (string? (get-in resp [:headers "date"])))))

(deftest returns-status-on-exceptional-responses
  (let [resp (request {:request-method :get :uri "/error"})]
    (is (= 500 (:status resp)))))
