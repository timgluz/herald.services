(ns herald.tests.api-utils-test
  (:require [midje.sweet :refer :all]
            [herald.core.sources.api-utils :as au]))

(facts "build-url"
  (fact "creates correct url with given url and path"
    (au/build-url "http://httpbin.org" nil)   => "http://httpbin.org"
    (au/build-url "http://httpbin.org" "")    => "http://httpbin.org"
    (au/build-url "http://httpbin.org" "/")   => "http://httpbin.org/"
    (au/build-url "http://httpbin.org" "/ip") => "http://httpbin.org/ip")
  (fact "creates correct url with additional query-params"
    (au/build-url "http://a.com" "" {:b 1})       => "http://a.com?b=1"
    (au/build-url "http://a.com" "" {:b 1 :c 3})  => "http://a.com?b=1&c=3"))

(facts "parse-url"
  (fact "parses url string into correct pieces"
    (let [url-items (au/parse-url "http://httpbin.org/ip?ip6=true")]
      (:protocol url-items) => "http"
      (:host url-items)     => "httpbin.org"
      (:path url-items)     => "/ip"
      (:query url-items)    => {"ip6" "true"})))

(facts "api-url-builder"
  (fact "built constructor builds correct url to the API resources."
    (let [url-builder (au/api-url-builder "https://api.gotham.com")]
      (url-builder "ping")    => "https://api.gotham.com/ping"
      (url-builder ["ping"])  => "https://api.gotham.com/ping"
      (url-builder ["repos" "timgluz"]) => "https://api.gotham.com/repos/timgluz"
      ;;it should also accept additional query-params
      (url-builder ["feed"] :since "today") => "https://api.gotham.com/feed?since=today")))

(facts "build-request-map"
  (fact "make correct data-map for get request"
    (let [req-map (au/build-request-map :get "https://a.io" "123")]
      (:method req-map) => :get
      (:url req-map) => "https://a.io"))
  (fact "builds correct request-map for post method"
    (let [req-map (au/build-request-map :post "https://a.io" "123"
                                        {:query-params {:ref "master"}
                                         :form-params {:b 42}})]
      (:method req-map)       => :post
      (:url req-map)          => "https://a.io"
      (:query-params req-map) => {:ref "master"}
      (:form-params req-map)  => {:b 42})))

(facts "process-response"
  (fact "it catches a response for a failed requests"
    (au/process-response {:error "404"}
      => {:error "404"})))

;;TODO: 0.7.2 - doesnt work with newest clj-http
(facts "request-api-resource"
  (fact "makes proper request against mocked Httpbin api"
    (let [resp (au/request-api-resource :get "http://httpbin.org"
                                        ["/status" 200] "none" {})]
       (:status resp) => 200
       (:body resp)   => "")))


