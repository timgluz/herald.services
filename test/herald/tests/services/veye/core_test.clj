(ns herald.tests.services.veye.core-test
  (:require [midje.sweet :refer :all]
            [blancas.morph.monads :refer [left? run-left right? run-right]]
            [herald.services.clients :refer [make-client]]
            [herald.services.veye.core :as veye]))

(def secret-key "ba7d93beb5de7820764e")
(def project-key "lein_project_clj_1")
(def test-filepath "test/herald/files/Gemfile.lock")
(def test-filename "Gemfile.lock")

(facts "search"
  (let [client (make-client :veye {:key "test" :secret secret-key} {})]
    (fact "returns properly coerced response"
      (let [resp (veye/search client "veye" {})
            dt (run-right resp)]
        (right? resp)           => true
        (map? dt)               => true
        (contains? dt :paging)  => true
        (empty? (:data dt))     => false
        (get-in dt [:data 0 :name])  => "veye"))))

(facts "get-projects"
  (let [client (make-client :veye {:key "test" :secret secret-key} {})]
    (fact "returns properly coerced data"
      (let [resp (veye/get-projects client)
            dt (run-right resp)]
        (right? resp)               => true
        (map? dt)                   => true
        (contains? dt :data)        => true
        (get-in dt [:data 0 :name]) => "droplocally2"
        ))))

(facts "get-project"
  (let [client (make-client :veye {:key "test" :secret secret-key} {})]
    (fact "returns properly coerced project info with dependencies"
      (let [resp (veye/get-project client project-key)
            dt (run-right resp)]
        (right? resp)             => true
        (map? dt)                 => true
        (contains? dt :data)      => true
        (get-in dt [:data :name]) => "droplocally2"
        (count (get-in dt [:data :dependencies])) => 22))))

(facts "create-project"
  (let [client (make-client :veye {:key "test" :secret secret-key} {})]
    (fact "creates new project from string and returns correct project' info"
      (let [file-content (slurp test-filepath)
            resp (veye/create-project client test-filename file-content)
            dt (run-right resp)]
        (right? resp)             => true
        (map? dt)                 => true
        (contains? dt :data)      => true
        (get-in dt [:data :name]) => #"^\w+Gemfile.lock$"
        (count (get-in dt [:data :dependencies])) => 7
        ;;-- delete created project
        (let [project-key (get-in dt [:data :project_key])
              resp (veye/delete-project client project-key)]
          (right? resp) => true
          (-> resp run-right :data) => true)))))

(facts "delete-project"
  (let [client (make-client :veye {:key "test" :secret secret-key} {})]
    (fact "deletes just created project successfully"
      (let [file-content (slurp test-filepath)
            resp (veye/create-project client test-filename file-content)]
        (right? resp)     => true
        (run-right resp)  => map?

        (let [new-project (run-right resp)
              project-key (get-in new-project [:data :project_key])
              resp (veye/delete-project client project-key)]
          (right? resp)               => true
          (-> resp run-right :data)   => true)))))

(facts "update-project"
  (let [client (make-client :veye {:key "test" :secret secret-key} {})]
    (fact "updates existing project and returns updated project file"
      (let [file-content (slurp test-filepath)
            resp (veye/create-project client test-filename file-content)
            proj-dt (-> resp run-right :data)]
        (right? resp)   => true
        (map? proj-dt)  => true
        (if-let [project-key (:project_key proj-dt)]
          (let [resp2 (veye/update-project client project-key test-filename file-content)
                dt2 (run-right resp2)]
            (right? resp2)  => true
            (map? dt2)      => true
            (get-in dt2 [:data :name]) => (get proj-dt :name)
            (get-in dt2 [:data :project_key]) => project-key)
          1 => 2)
        ;;-- delete create project
        (let [resp (veye/delete-project client (:project_key proj-dt))]
          (right? resp) => true
          (-> resp run-right :data) => true)))))

