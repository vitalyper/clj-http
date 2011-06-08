(ns clj-http.run-server
  (:use 
    [clojure.test]
    [ring.adapter.jetty])
  (:require 
    [clojure.contrib.pprint :as pp]
    [clojure.contrib.io :as io])
)

(def *jetty* nil)

(defn handler [req]
  (pp/pprint req)
  (println) (println)
  (condp = [(:request-method req) (:uri req)]
    [:get "/get"]
      {:status 200 :body "get"}
    [:head "/head"]
      {:status 200}
    [:get "/content-type"]
      {:status 200 :body (:content-type req)}
    [:get "/header"]
      {:status 200 :body (get-in req [:headers "x-my-header"])}
    [:post "/post"]
      {:status 200 :body (io/slurp* (:body req))}
    [:get "/error"]
      {:status 500 :body "o noes"}))

(defn 
  before-once 
  "clojure.test once fixture"
  [f]
  (start-jetty)
  (stop-jetty)
)

(defn start-jetty []
  (if (nil? *jetty*)
    (do 
      (def *jetty*
        (run-jetty
          (-> #'handler )
          {:port 8080, :join? false}))
      (println "started jetty"))
    (println "jetty is already running")))

(defn stop-jetty []
  (if (nil? *jetty*)
    (println "jetty has not been started. use start_jetty to start.")
    (do 
      (. *jetty* stop)
      (def *jetty* nil)
      (println "jetty has been stopped."))))
