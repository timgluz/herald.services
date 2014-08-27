(ns herald.services.github.core
  (:require [schema.core :as s]
            [schema.macros :as sm]
            [blancas.morph.monads :refer [either left right]]
            [clojure.string :as string]
            [taoensso.timbre :as log]
            [herald.services.clients :as clients]
            [herald.services.schemas :as schemas :refer [response-walker]])
  (:import [herald.services.schemas SRError SRPagedEntity SREntity]
           [herald.services.clients GithubClient]
           [blancas.morph.monads Either]))


;;-- COERCERS
;- user entity coercer
(defn matches-schema?
  [schema block]
  (and (map? block)
       ((comp empty? s/check) schema block)))

;- matcher schema for multimethod dispatch
(def GithubUser {:login s/Str
                 :type s/Str
                 s/Keyword s/Any})


(defmulti coerce-user-item
  (fn [block]
    (if (matches-schema? GithubUser block)
      :entity
      :default)))

(defmethod coerce-user-item :entity [block]
  {:name (:login block)
   :username (:login block)
   :type (-> block :type str string/lower-case)})

(defmethod coerce-user-item :default [block]
  block)

;- search coercer
(def GithubRepoPermission {:admin s/Bool
                           :pull s/Bool
                           :push s/Bool})

(def GithubRepoOwner {:login s/Str
                      :type s/Str
                      s/Keyword s/Any})

(def GithubRepoItem {:id s/Int
                     :name s/Str
                     :full_name s/Str
                     :html_url s/Str
                     :git_url s/Str
                     :description s/Str
                     :language s/Str
                     :default_branch s/Str
                     :private s/Bool
                     :fork s/Bool
                     :owner GithubRepoOwner
                     s/Keyword s/Any})

(defmulti coerce-repo-item
  (fn [block]
    (cond
      (matches-schema? GithubRepoPermission block) :permission
      (matches-schema? GithubRepoOwner block) :owner
      (matches-schema? GithubRepoItem block) :repo
      :else :default)))

(defmethod coerce-repo-item :default [block]
  block)

(defmethod coerce-repo-item :owner [block]
  {:login (:login block)
   :type (-> block :type str string/lower-case)})

(defmethod coerce-repo-item :permission [block]
  {:admin (get block :admin false)
   :pushable (get block :push false)
   :pullable (get block :pull false)})

(defmethod coerce-repo-item :repo [block]
  {:scm_id (-> block :id str)
   :name (:name block)
   :fullname (:full_name block)
   :description (:description block)
   :html_url (:html_url block)
   :scm_url (:git_url block)
   :language (:language block)
   :default_branch (:default_branch block)
   :owner_login (get-in block [:owner :login])
   :private (:private block)
   :fork (:fork block)
   :admin (get-in block [:permission :admin])
   :pushable (get-in block [:permission :pushable])
   :pullable (get-in block [:permission :pullable])
   })

;- response coercers
(def GithubListResponse {:items [GithubRepoItem]
                         (s/optional-key :total_count) s/Int
                         (s/optional-key :incomplete_results) s/Bool
                         s/Keyword s/Any})

(defmulti coerce-response (fn [type _] type))

(defmethod coerce-response ::raw [_ response]
  (get response :body {}))

(defmethod coerce-response ::user [_ response]
  ((response-walker GithubUser coerce-user-item)
   (get response :body {})))

(defmethod coerce-response ::search [_ response]
  ((response-walker GithubListResponse coerce-repo-item)
   (get response :body {})))

(defmethod coerce-response :default [type response]
  (log/error "Github/core not supported coercer: " type))

;;-- API functions

(sm/defn get-current-user :- Either
  "Fetches user information about auth key owners"
  [client :- GithubClient]
  (either [resp (clients/rpc-call client :get "user")]
    (left resp)
    (if-let [coerced-dt (coerce-response ::user resp)]
      (right (SREntity. coerced-dt))
      (left (SRError. 503 "Coercing error - get-current-user" resp)))))

(def SearchOptions {(s/optional-key :sort) s/Str
                    (s/optional-key :order) (s/enum "asc" "desc")})

(sm/defn search :- Either
  "Search repositories on Github"
  [client :- GithubClient
   term :- s/Str
   options :- SearchOptions]
  (either [resp (clients/rpc-call client
                                  :get "/search/repositories"
                                  (merge {:q term} options))]
    (left resp)
    (if-let [coerced-dt (coerce-response ::search resp)]
      (right (SREntity. (:items coerced-dt)))
      (left (SRError. 503 "Coercing error - search" resp)))))

(comment
  (require '[herald.services.clients :as clients :refer [make-client]] :reload)
  (require '[herald.services.github.core :as git] :reload)

  (def token "7790a09b31c734f8581d581aa33bb8ece1e7149f")
  (def client (make-client :github {:key "test" :secret token} {}))

  (git/get-current-user client)
  (git/search client "veye" {})
  )

