(ns herald.tests.services.clients-test
  (:require [midje.sweet :refer :all]
            [blancas.morph.monads :refer [right? run-right]]
            [herald.services.clients :as clients]))

(def test-url "https://www.versioneye.com/api/v2")
(def test-token "ba7d93beb5de7820764e")
(def test-token2 "7790a09b31c734f8581d581aa33bb8ece1e7149f")

(facts "build-url"
  (fact "builds a correct url if path is string"
    (clients/build-url test-url "me" nil) => "https://www.versioneye.com/api/v2/me"
    (clients/build-url test-url "/me" nil) => "https://www.versioneye.com/me")
  (fact "builds a correct url if path is Number"
    (clients/build-url test-url 10 nil) => (str test-url "/10")
    (clients/build-url test-url 1.0 nil) => (str test-url "/1.0"))
  (fact "builds a currect url if path is Keyword"
    (clients/build-url test-url :me nil) => (str test-url "/me")
    (clients/build-url test-url :product/search nil) => (str test-url "/search"))
  (fact "build a correct url if path is list of strings&numbers"
    (clients/build-url test-url ["users"] nil) => (str test-url "/users")
    (clients/build-url test-url ["users" 1] nil) => (str test-url "/users/1")
    (clients/build-url test-url ["users" 1 "mails"] nil) => (str test-url "/users/1/mails")))


(facts "build-request"
  (let [client (clients/make-client :veye {:secret ""} {})]
    (fact "builds a correct request map for get method"
      (clients/build-request client :get "me" {} {:debug true})
      => {:method :get
          :url (str test-url "/me")
          :debug true
          :as :json
          :coerce :unexceptional
          :conn-timeout 1000
          :headers  {"User-Agent"  "Herald (info@versioneye.com)"}
          :socket-timeout 1000
          :throw-entire-message? true})))

(facts "make-client"
  (fact "builds a working Veye client"
    (let [client (clients/make-client :veye {:secret test-token} {})
          resp (clients/rpc-call client :get "services/ping")]
      (right? resp) => true
      (let [dt (run-right resp)]
        (:status dt) => 200
        (:body dt) => {:success true :message "pong"})))
  (fact "build a Github client"
    (let [client (clients/make-client :github {:secret test-token2} {})
          resp (clients/rpc-call client :get :user)]
      (right? resp)
      (let [dt (run-right resp)]
        (:status dt) => 200
        (-> dt :body :login) => "heraldtest"))))

