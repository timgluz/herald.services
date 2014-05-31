(ns herald.tests.github-test
  (:require [midje.sweet :refer :all]
            [herald.core.sources.github :as github]))

(def test-token "ca9a2c6d6ac48eb3f570e0d6177fd0e43b2dd4d3")
(def test-user-id "heraldtest")
(def test-repo "heraldtest/fantom_hydra")
(def test-sha "20b9c1193a16c1d86f2a524d30c3e37bd0050bc4")

(facts "get-resource"
  (fact "get correct response"
    (let [user (github/get-resource ["/user"] test-token)]
      user   =not=> nil
      (:login user) => test-user-id))
  (fact "accepts extra-params"
    (let [user (github/get-resource ["/user"] test-token
                                    {:query-params {:show :all}})]
      user =not=> nil
      (:login user) => test-user-id)))

(facts "get-current-user"
  (fact "gets proper and parsed content"
    (let [resp (github/get-current-user test-token)]
      (:error resp) => nil
      (:login resp) => test-user-id)))

(facts "get-last-page-number"
  (fact "parses correct page number from url"
    (let [links {:last {:href "http://b.org/api?page=42"}}]
      (github/get-last-page-number links) => 42)))

(facts "get-user-repos"
  (fact "returns correct amount of repos"
    (let [repos (github/get-user-repos test-user-id test-token)]
      (count repos) => 4
      (-> repos first :name) => "veye")))

(facts "get-user-orgs"
  (fact "returns correct user-orgs"
    (let [orgs (github/get-user-orgs test-user-id test-token)]
      (count orgs) => 1
      (-> orgs first :login) => "tauho")))

(facts "get-org-repos"
  (fact "returns correct repos from org the user belongs"
    (let [repos (github/get-org-repos "tauho" test-token)]
      (count repos) => 8
      (-> repos first :name) => "weebu")))

(facts "get-repo-branches"
  (fact "returns correct branches for test repo"
    (let [branches (github/get-repo-branches test-repo test-token)
          names (set (map :name branches))]
      (count branches) => 3
      (contains? names "master") => true
      (contains? names "clojure_branch") => true)))

(facts "get-repo-tree"
  (fact "returns correct repo tree"
    (let [repo-tree (github/get-repo-tree test-repo test-sha test-token)]
      (contains? repo-tree :tree) => true)))

(facts "get-file-content"
  (fact "returns file content of the README file"
    (let [file-doc (github/get-file-content test-repo  "master" "README.md" test-token)]
      (:type file-doc) => "file"
      (:size file-doc) => 139
      (:name file-doc) => "README.md")))

