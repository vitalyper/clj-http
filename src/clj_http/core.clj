(ns clj-http.core
  "Core HTTP request/response implementation."
  (:import (org.apache.http HttpRequest HttpEntityEnclosingRequest HttpResponse Header HttpHost))
  (:import (org.apache.http.util EntityUtils))
  (:import (org.apache.http.client HttpClient))
  (:import (org.apache.http.entity ByteArrayEntity))
  (:import (org.apache.http.client.methods HttpGet HttpHead HttpPut HttpPost HttpDelete))
  (:import (org.apache.http.client.params CookiePolicy ClientPNames))
  (:import (org.apache.http.impl.client DefaultHttpClient))
  (:import (org.apache.http.params BasicHttpParams HttpConnectionParams
                                   HttpParams HttpProtocolParams))
  (:import (org.apache.http.conn.scheme PlainSocketFactory Scheme
                                        SchemeRegistry SocketFactory))
  (:import (org.apache.http.conn.ssl SSLSocketFactory))
  (:import (org.apache.http.impl.conn.tsccm ThreadSafeClientConnManager))
  (:import (org.apache.http.conn.params ConnRoutePNames))
  (:import (java.util.concurrent TimeUnit))
  (:import (java.io InputStream)))

(defn- parse-headers [^HttpResponse http-resp]
  (into {} (map (fn [^Header h] [(.toLowerCase (.getName h)) (.getValue h)])
                (iterator-seq (.headerIterator http-resp)))))

(defn pooled-http-client
  ([] (pooled-http-client {:ttl 120
                           :max-total-conns 200
                           :max-per-route 10
                           :use-proxy false}))
  ([{:keys [ttl max-total-conns max-per-route use-proxy]}]
     (let [psf (PlainSocketFactory/getSocketFactory)
           ssf (SSLSocketFactory/getSocketFactory)
           schemes (doto (SchemeRegistry.)
                     (.register (Scheme. "http" psf 80))
                     (.register (Scheme. "https" ssf 443)))
           mgr (doto (ThreadSafeClientConnManager. schemes (long ttl) TimeUnit/SECONDS)
                 (.setMaxTotal max-total-conns)
                 (.setDefaultMaxPerRoute max-per-route))
           http-client (DefaultHttpClient. mgr)]
       (doto (.getParams http-client)
         (.setParameter ClientPNames/COOKIE_POLICY CookiePolicy/BROWSER_COMPATIBILITY)
         (.setParameter ClientPNames/HANDLE_REDIRECTS false))
       (if use-proxy
         (doto (.getParams http-client)
           (.setParameter ConnRoutePNames/DEFAULT_PROXY 
              (HttpHost. 
                (System/getProperty "http.proxyHost" "localhost")
              ; default to Fiddler's port
              (Integer/parseInt (System/getProperty "http.proxyPort" "8888"))))))
       http-client)))

(defn basic-http-client
  []
  (let [http-client (DefaultHttpClient.)
        params (.getParams http-client)]
    (try
      (doto (.getParams http-client)
        (.setParameter ClientPNames/COOKIE_POLICY CookiePolicy/BROWSER_COMPATIBILITY)
        (.setParameter ClientPNames/HANDLE_REDIRECTS false)))
    http-client))

(defn request
  "Executes the HTTP request corresponding to the given Ring request map and
   returns the Ring response map corresponding to the resulting HTTP response."
  ([^HttpClient http-client {:keys [request-method scheme server-name server-port uri query-string
                                    headers content-type character-encoding body]}]
     (try
       (let [http-url (str scheme "://" server-name
                           (if server-port (str ":" server-port))
                           uri
                           (if query-string (str "?" query-string)))
             ^HttpRequest
             http-req (case request-method
                            :get    (HttpGet. http-url)
                            :head   (HttpHead. http-url)
                            :put    (HttpPut. http-url)
                            :post   (HttpPost. http-url)
                            :delete (HttpDelete. http-url))]
         (if (and content-type character-encoding)
           (.addHeader http-req "Content-Type"
                       (str content-type "; charset=" character-encoding)))
         (if (and content-type (not character-encoding))
           (.addHeader http-req "Content-Type" content-type))
         (.addHeader http-req "Connection" "close")
         (doseq [[header-n header-v] headers]
           (.addHeader http-req header-n header-v))
         (if body
           (let [http-body (ByteArrayEntity. body)]
             (.setEntity ^HttpEntityEnclosingRequest http-req http-body)))
         (let [http-resp (.execute http-client http-req)
               resp {:status (.getStatusCode (.getStatusLine http-resp))
                     :headers (parse-headers http-resp)
                     :body (when-let [ent (.getEntity http-resp)]
                             (.getContent ent))}]
           resp))))
  ([config]
     (request (basic-http-client) config)))
