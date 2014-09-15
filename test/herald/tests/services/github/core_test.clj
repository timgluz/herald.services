(ns herald.tests.services.github.core-test
  (:require [midje.sweet :refer :all]
            [blancas.morph.monads :refer [right? run-right]]
            [herald.services.clients :refer [make-client]]
            [herald.services.github.core :as git]))

(def secret-key "7790a09b31c734f8581d581aa33bb8ece1e7149f")
(def test-user "heraldtest")

(def test-links "<https://api.github.com/user/repos?page=3&per_page=100>; rel=\"next\",
<https://api.github.com/user/repos?page=50&per_page=100>; rel=\"last\"")

(facts "parse-pagination"
  (fact "parses content of Github link headers correctly"
    (let [dt (git/parse-pagination test-links)]
      dt => {:next "https://api.github.com/user/repos?page=3&per_page=100"
             :last "https://api.github.com/user/repos?page=50&per_page=100"}))
  (fact "returns `nil` if data is partial or missing"
    (git/parse-pagination nil) => nil
    (git/parse-pagination "")  => nil
    (git/parse-pagination "jiberish blaberish") => nil))

(facts "coerce-pagination"
  (fact "returns correct pagination table"
    (git/coerce-pagination 1 {:headers {"Link" test-links}})
     => {:current 1
         :per-page 100
         :total 50
         :total-items 5000})
  (fact "returns a default pagination table if there's no pagination header"
    (git/coerce-pagination 1 {:headers {}})
    => {:current 1
        :per-page 30
        :total 1
        :total-items 1}))

(facts "get-current-user"
  (let [client (make-client :github {:key "test" :secret secret-key} {})]
    (fact "returns correct user info"
      (let [resp (git/get-current-user client)
            dt (run-right resp)]
        (right? resp) => true
        (map? dt)     => true
        (get-in dt [:data :username]) => test-user
        (get-in dt [:data :type])     => "user"))))

(facts "get-user-orgs"
  (let [client (make-client :github {:key "test" :secret secret-key} {})]
    (fact "returns correct list of user's organizations"
      (let [resp (git/get-user-orgs client 1)
            dt (run-right resp)]
        (right? resp) => true
        (map? dt)     => true
        (count (:data dt)) => 1
        (get-in dt [:data 0 :username]) => "tauho"
        (:paging dt)  => {:current 1
                          :per-page 30
                          :total 1
                          :total-items 1}))))

(facts "search"
  (let [client (make-client :github {:key "test" :secret secret-key} {})]
    (fact "returns correct search results"
      (let [resp (git/search client "veye" {})
            dt (run-right resp)]
        (right? resp) => true
        (map? dt)     => true
        (contains? dt :data)  => true
        (count (:data dt))    => 2))))

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

(def test-repo "heraldtest/fantom_hydra")
(def test-sha "20b9c1193a16c1d86f2a524d30c3e37bd0050bc4")
(def file-sha "a12f4068eabccd620d10f55194770c76adaebc1e")

(facts "get-repo-branches"
  (let [client (make-client :github {:key "test" :secret secret-key} {})]
    (fact "returns a correct list of branches with commit sha"
      (let [resp (git/get-repo-branches client test-repo 1)
            dt (run-right resp)]
        (right? resp) => true
        (map? dt)     => true
        (-> dt :data count) => 3
        (-> dt :data first :name) => "clojure_branch"
        (get-in dt [:paging]) => {:current 1
                                  :per-page 30
                                  :total 1
                                  :total-items 1}))))

(facts "get-repo-tree"
  (let [client (make-client :github {:key "test" :secret secret-key} {})]
    (fact "returns a correct list of tree-items"
      (let [resp (git/get-repo-tree client test-repo test-sha)
            dt (run-right resp)]
        (right? resp) => true
        (map? dt)     => true
        (-> dt :data count) => 4
        (-> dt :data first :name) => "Gemfile"))))

(facts "get-file-content"
  (let [client (make-client :github {:key "test" :secret secret-key} {})]
    (fact "returns a correct data-map with decoded file content"
      (let [resp (git/get-file-content client test-repo file-sha)
            dt (run-right resp)]
        (right? resp) => true
        (map? dt)     => true
        (-> dt :data :commit_sha) => file-sha
        (-> dt :data :size) => 1990))))

