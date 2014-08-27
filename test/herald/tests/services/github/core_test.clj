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

