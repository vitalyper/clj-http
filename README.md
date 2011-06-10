# clj-http
clj-http based on mmcgrana/clj-http and getwoven/clj-http, etc.

## Changes since mmcgrana/clj-http:
1. Pooled client via apache HttpClient ThreadSafeClientConnManager.
2. Http proxy support via ConnRoutePNames/DEFAULT_PROXY.
3. Added some unit tests. Changed client_test.clj and core_test.clj use clojure.test fixtures.

## Usage
Backward compatible with original implementation.

To get pooled client with http proxy support:
Set system properties "http.proxyHost" and "http.proxyPort".

    (clj-http.core/pooled-http-client 
        {:ttl 120 :max-total-conns 200 :max-per-route 10 :use-proxy true})

## To run tests
    (use 'clojure.test)
    (run-tests 'clj-http.client-test 'clj-http.core-test)

## License
Distributed under the Eclipse Public License, the same as Clojure.
