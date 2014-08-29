# herald.core.scm

A Clojure library designed to consume various API endpoint and coerce responses to unified schema.

## Usage

#### VersionEye consumer
```
(require '[herald.services.clients :refer [make-client] :as clients] :reload)
(require '[herald.services.veye.core :as veye] :reload)

(def client (make-client :veye {:key "aaa" 
							  :secret "<add own VersionEye token>"} {}))
(veye/search client "veye" {})
(veye/get-projects client)
(veye/get-project client "lein_project_clj_1")

(def file-content (slurp "test/herald/files/Gemfile.lock"))
(veye/create-project client "Gemfile.lock" file-content)
```


#### Github consumer

```
(require '[herald.services.clients :as clients :refer [make-client]] :reload)
(require '[herald.services.github.core :as git] :reload)

(def token "<add own github token>")
(def client (make-client :github {:key "test" :secret token} {}))

(git/get-current-user client)
(git/get-user-orgs client 1)

(git/search client "veye" {})
(git/get-user-repos client 1)
(git/get-org-repos client "tauho" 1)

(def test-repo "heraldtest/fantom_hydra")
(def test-sha "20b9c1193a16c1d86f2a524d30c3e37bd0050bc4")
(git/get-repo-branches client test-repo 1)
(git/get-repo-tree client test-repo test-sha)
(git/get-file-content client test-repo test-sha "Gemfile")
 
 ```
  
## License

Copyright © 2014 Timo Sulg, VersionEye GMBH 

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
