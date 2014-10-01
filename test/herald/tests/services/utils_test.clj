(ns herald.tests.services.utils-test
  (:require [midje.sweet :refer :all]
            [herald.services.utils :refer [build-url build-request]]
            [herald.services.clients :as clients]))

(def test-url "https://www.versioneye.com/api/v2")

(facts "build-url"
  (fact "builds a correct url if path is string"
    (build-url test-url "me" nil) => "https://www.versioneye.com/api/v2/me"
    (build-url test-url "/me" nil) => "https://www.versioneye.com/me")
  (fact "builds a correct url if path is Number"
    (build-url test-url 10 nil) => (str test-url "/10")
    (build-url test-url 1.0 nil) => (str test-url "/1.0"))
  (fact "builds a currect url if path is Keyword"
    (build-url test-url :me nil) => (str test-url "/me")
    (build-url test-url :product/search nil) => (str test-url "/search"))
  (fact "build a correct url if path is list of strings&numbers"
    (build-url test-url ["users"] nil) => (str test-url "/users")
    (build-url test-url ["users" 1] nil) => (str test-url "/users/1")
    (build-url test-url ["users" 1 "mails"] nil) => (str test-url "/users/1/mails")))


(facts "build-request"
  (fact "builds a correct request map for get method"
    (let [client (clients/make-client :veye {:secret "123"} {})]
      (build-request client :get "me" {} {:debug true})
      => {:method :get
          :url (str test-url "/me")
          :debug true
          :as :json
          :coerce :unexceptional
          :conn-timeout 2500
          :headers  {"User-Agent"  "Herald (info@versioneye.com)"}
          :socket-timeout 5000
          :throw-entire-message? true})))

