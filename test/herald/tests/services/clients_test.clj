(ns herald.tests.services.clients-test
  (:require [midje.sweet :refer :all]
            [blancas.morph.monads :refer [right? run-right]]
            [herald.services.clients :as clients]))

(def test-url "https://www.versioneye.com/api/v2")
(def test-token "ba7d93beb5de7820764e")
(def test-token2 "7790a09b31c734f8581d581aa33bb8ece1e7149f")

(facts "make-client"
  (fact "builds a working Veye client"
    (let [client (clients/make-client :veye {:secret test-token} {})
          resp (.rpc-call client :get "services/ping")]
      (right? resp) => true
      (let [dt (run-right resp)]
        (:status dt) => 200
        (:body dt) => {:success true :message "pong"})))

  (fact "build a Github client"
    (let [client (clients/make-client :github {:secret test-token2} {})
          resp (.rpc-call client :get :user)]
      (right? resp)
      (let [dt (run-right resp)]
        (:status dt) => 200
        (-> dt :body :login) => "heraldtest"))))

