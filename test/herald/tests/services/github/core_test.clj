(ns herald.tests.services.github.core-test
  (:require [midje.sweet :refer :all]
            [blancas.morph.monads :refer [right? run-right]]
            [herald.services.clients :refer [make-client]]
            [herald.services.github.core :as git]))

(def secret-key "7790a09b31c734f8581d581aa33bb8ece1e7149f")
(def test-user "heraldtest")

(facts "get-current-user"
  (let [client (make-client :github {:key "test" :secret secret-key} {})]
    (fact "returns correct user info"
      (let [resp (git/get-current-user client)
            dt (run-right resp)]
        (right? resp) => true
        (map? dt)     => true
        (get-in dt [:data :username]) => test-user
        (get-in dt [:data :type])     => "user"))))


(facts "search"
  (let [client (make-client :github {:key "test" :secret secret-key} {})]
    (fact "returns correct search results"
      (let [resp (git/search client "veye" {})
            dt (run-right resp)]
        (right? resp) => true
        (map? dt)     => true
        (contains? dt :data)  => true
        (count (:data dt))    => 2
        ))))

(facts "get-user-repos"
  (let [client (make-client :github {:key "test" :secret secret-key} {})]
    (fact "returns correct results for page.1"
      (let [resp (git/get-user-repos client 1)
            dt (run-right resp)]
        (right? resp) => true
        (map? dt)     => true
        (count (:data dt)) => 4
        (get-in dt [:paging]) => {:current 1
                                  :per-page 30
                                  :total 1
                                  :total-items 1}))))
(facts "get-org-repos"
  (let [client (make-client :github {:key "test" :secret secret-key} {})]
    (fact "returns correct results for page.1"
      (let [resp (git/get-org-repos client "tauho" 1)
            dt (run-right resp)]
        (right? resp) => true
        (map? dt)     => true
        (count (:data dt)) => 8
        (get-in dt [:paging]) => {:current 1
                                  :per-page 30
                                  :total 1
                                  :total-items 1}))))


(facts "get-org-repos")
