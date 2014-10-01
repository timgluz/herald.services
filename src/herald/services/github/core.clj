(ns herald.services.github.core
  (:require [schema.core :as s]
            [schema.macros :as sm]
            [blancas.morph.monads :refer [either left right]]
            [clojure.string :as string]
            [cemerick.url :refer [url]]
            [taoensso.timbre :as log]
            [base64-clj.core :as base64]
            [herald.services.impl :refer [IRPC_Client IAuthorizer]]
            [herald.services.utils :refer [build-url do-request build-request]]
            [herald.services.schemas :as schemas
                                     :refer [response-walker matches-schema?
                                             ->SRError ->SRResponse
                                             ->SRPagedEntity ->SREntity]])
  (:import [blancas.morph.monads Either]))

(sm/defrecord GithubClient
  [api-url :- s/Str
   auth :- schemas/Auth
   client-opts :- schemas/ClientOptions]
  IAuthorizer
  (append-auth [this request-dt]
    (assoc-in request-dt
              [:headers "Authorization"]
              (format "token %s" (get-in this [:auth :secret]))))
  IRPC_Client
  (rpc-call [this method path]
    (.rpc-call this method path {} {}))
  (rpc-call [this method path query-params]
    (.rpc-call this method path query-params {}))
  (rpc-call [this method path query-params extra-client-opts]
    (let [request-dt (build-request this method path query-params extra-client-opts)]
      (either [resp (do-request (.append-auth this request-dt))]
        (do
          (log/error "GithubClient.rpc-call failed: " path "\n" resp)
          (left (->SRError (:status resp)
                          (str "Github client cant access resource: " path)
                          resp)))
        (if (< 199 (:status resp) 300)
          (right (->SRResponse (:status resp) (:headers resp) (:body resp)))
          (left (->SRError (:status resp)
                          (str "GithubClient.rpc-call Request failed")
                          resp)))))))


;;-- COERCERS
;- matcher schema for multimethod dispatch
(def GithubUser {:login s/Str
                 (s/optional-key :type) s/Str
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
                     :language (s/either s/Str (s/pred nil?))
                     :default_branch s/Str
                     :private s/Bool
                     :fork s/Bool
                     :owner GithubRepoOwner
                     s/Keyword s/Any})

(def GithubRepoBranch {:name s/Str
                       :commit {:sha s/Str
                                :url s/Str}})

(def GithubRepoTreeItem {:path s/Str
                         :mode s/Str
                         :type s/Str
                         :sha s/Str
                         :url s/Str
                         (s/optional-key :size) s/Int
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
  {:scm_id    (-> block :id str)
   :name      (:name block)
   :fullname  (:full_name block)
   :description (:description block)
   :html_url  (:html_url block)
   :scm_url   (:git_url block)
   :language  (str (:language block))
   :default_branch (:default_branch block)
   :owner_login (get-in block [:owner :login])
   :owner_type (get-in block [:owner :type])
   :private   (:private block)
   :fork      (:fork block)
   :admin     (get-in block [:permissions :admin])
   :pushable  (get-in block [:permissions :pushable])
   :pullable  (get-in block [:permissions :pullable])})

;;-- repo branch coercer
(defmulti coerce-repo-branch
  (fn [block]
    (if (matches-schema? GithubRepoBranch block)
      :branch
      :default)))

(defmethod coerce-repo-branch :branch [block]
  {:name (-> block :name str)
   :commit_sha (-> block :commit :sha str)})

(defmethod coerce-repo-branch :default [block]
  block)

;;-- repo tree coercer
(defmulti coerce-repo-tree
  (fn [block]
    (if (matches-schema? GithubRepoTreeItem block)
      :tree-item
      :default)))

(defmethod coerce-repo-tree :tree-item [block]
  (let [filename (-> block :path str (string/split #"\/") last)]
    {:name filename
     :path (:path block)
     :commit_sha (:sha block)
     :size (:size block)
     :url (:url block)}))

(defmethod coerce-repo-tree :default [block]
  block)

;;-- repo-file coercer
(def GithubFileContent {:sha s/Str
                        :encoding s/Str
                        :content s/Str
                        :size s/Int
                        s/Keyword s/Any})

(defmulti coerce-file-content
  (fn [block]
    (if (matches-schema? GithubFileContent block)
      :file
      :default)))

(defmethod coerce-file-content :file [block]
  (let [file-content (-> block
                         :content str
                         (string/replace #"[^A-Za-z0-9\+\/\=]"  "")
                         (base64/decode "UTF-8"))]
    {:commit_sha (:sha block)
     :size (:size block)
     :encoding "str"
     :content file-content}))

(defmethod coerce-file-content :default [block] block)

;;-- Pagination coercer
(defn parse-pagination
  "parses github pagination links from response[:headers][\"Links\"]"
  [links]
  (apply merge
    (map
      (fn [link]
        (when-let [matches (re-find #"\<(\S+)\>;\s+rel=\"(\w+)\"" link)]
          (apply #(hash-map (keyword %2) %1)
                 (rest matches))))
      (string/split (str links) #"\,"))))

(defn coerce-pagination
  [current-page resp]
  (let [links (get-in resp [:headers "Link"])
        link-dt (parse-pagination links)]
    (if (re-matches #"https?:\/\/\S+" (str (:last link-dt)))
      (let [last-url (url (:last link-dt))
            last-page (Integer. (get-in last-url [:query "page"]))
            per-page (Integer. (get-in last-url [:query "per_page"] "30"))]
        {:current current-page
          :per-page per-page
          :total last-page
          :total-items (* per-page last-page)})
      {:current current-page
       :per-page 30
       :total 1
       :total-items 1})))

;- response coercers
(def GithubSearchResponse {:items [GithubRepoItem]
                           :total_count s/Int
                           :incomplete_results s/Bool
                           s/Keyword s/Any})

(def GithubEntityList [(s/either GithubRepoItem GithubRepoBranch GithubUser
                                 GithubRepoTreeItem)])

;;-- response coercers

(defmulti coerce-response (fn [type _] type))
(defmethod coerce-response ::raw [_ response]
  (get response :body {}))

(defmethod coerce-response ::user [_ response]
  ((response-walker GithubUser coerce-user-item)
    (get response :body {})))

(defmethod coerce-response ::user-orgs [_ response]
  ((response-walker GithubEntityList coerce-user-item)
    (get response :body [])))

(defmethod coerce-response ::search [_ response]
  ((response-walker GithubSearchResponse coerce-repo-item)
    (get response :body {})))

(defmethod coerce-response ::repos [_ response]
  ((response-walker GithubEntityList coerce-repo-item)
    (vec (get response :body []))))

(defmethod coerce-response ::branches [_ response]
  ((response-walker GithubEntityList coerce-repo-branch)
    (vec (get response :body []))))

(defmethod coerce-response ::repo-tree [_ response]
  ((response-walker GithubEntityList coerce-repo-tree)
    (get-in response [:body :tree] {})))

(defmethod coerce-response ::file-content [_ response]
  ((response-walker GithubFileContent coerce-file-content)
   (get response :body {})))

(defmethod coerce-response :default [type response]
  (log/error "Github/core not supported coercer: " type))

;;-- API functions
(sm/defn rate-limit :- Either
  "returns a hash-map with the latest limits;"
  [client :- GithubClient]
  (either [resp (.rpc-call client :get "rate_limit")]
    (left resp)
    (let [default-dt {:limit 0 :remaining 0 :reset 0}
          dt (get-in resp [:body :resources :core] default-dt)]
      (right dt))))

(sm/defn get-current-user :- Either
  "Fetches user information about auth key owners"
  [client :- GithubClient]
  (either [resp (.rpc-call client :get "user")]
    (left resp) ;; passes client error into caller-fn
    (if-let [coerced-dt (coerce-response ::user resp)]
      (right (->SREntity coerced-dt))
      (left (->SRError 503 "Coercing error - get-current-user" resp)))))

(sm/defn get-user-orgs :- Either
  "Returns list of organization objects, where user has access to."
  [client :- GithubClient
   page :- s/Int]
  (either [resp (.rpc-call client :get "/user/orgs")]
    (left resp)
    (if-let [coerced-dt (coerce-response ::user-orgs resp)]
      (right
        (->SRPagedEntity coerced-dt (coerce-pagination page resp)))
      (left (->SRError 503 "Coercing error - get-user-orgs" resp)))))

(def SearchOptions {(s/optional-key :sort) s/Str
                    (s/optional-key :order) (s/enum "asc" "desc")})

(sm/defn search :- Either
  "Search repositories on Github"
  [client :- GithubClient
   term :- s/Str
   options :- SearchOptions]
  (either [resp (.rpc-call client
                          :get "/search/repositories"
                          (merge {:q term} options))]
    (left resp)
    (if-let [coerced-dt (coerce-response ::search resp)]
      (right (->SREntity (:items coerced-dt)))
      (left (->SRError 503 "Coercing error - search" resp)))))

(sm/defn get-user-repos :- Either
  "returns paginated list of token owner's repos."
  [client :- GithubClient
   page :- s/Int]
  (either [resp (.rpc-call client :get "/user/repos" {:page page :type "all"})]
    (left resp)
    (if-let [coerced-dt (coerce-response ::repos resp)]
      (right
        (->SRPagedEntity coerced-dt (coerce-pagination page resp)))
      (left (->SRError 503 "Coercing error - get-user-repos" resp)))))

(sm/defn get-org-repos :- Either
  "returns list of organization's repos to which token's owner has access"
  [client :- GithubClient
   org-name :- s/Str
   page :- s/Int]
  (either [resp (.rpc-call client :get ["orgs" org-name "repos"] {:page page})]
    (left resp)
    (if-let [coerced-dt (coerce-response ::repos resp)]
      (right
        (->SRPagedEntity coerced-dt (coerce-pagination page resp)))
      (left (->SRError 503 "Coercing error - get-org-repos" resp)))))

(sm/defn get-repo-branches :- Either
  "returns list of repo branches"
  [client :- GithubClient
   repo   :- s/Str
   page   :- s/Int]
  (either [resp (.rpc-call client :get ["repos" repo "branches"])]
    (left resp) ;; pass client error
    (if-let [coerced-dt (coerce-response ::branches resp)]
      (right
        (->SRPagedEntity coerced-dt (coerce-pagination page resp)))
      (left (->SRError 503 "Coercing error - get-repo-branches" resp)))))

(sm/defn get-repo-tree
  [client :- GithubClient
   repo :- s/Str
   sha :- s/Str]
  (either [resp (.rpc-call client
                          :get ["repos" repo "git/trees" sha])
                          {:recursive 1}]
    (left resp)
    (if-let [coerced-dt (coerce-response ::repo-tree resp)]
      (right (->SREntity coerced-dt))
      (left (->SRError 503 "Coercing error - get-repo-tree" resp)))))


(sm/defn get-file-content
  "gets a content of blob by the path and ref-key;
  Here `ref-key` may be a name of branch, branch's commit-sha or tag id.
  Name of branch gives a latest version; sha or tag-id fixed version;"
  [client :- GithubClient
   repo :- s/Str
   file-sha :- s/Str]
  (either [resp (.rpc-call client :get ["repos" repo "git/blobs" file-sha])]
    (left resp)
    (if-let [coerced-dt (coerce-response ::file-content resp)]
      (right (->SREntity coerced-dt))
      (left (->SRError 503 "Coercing error - get-file-fontent" resp)))))

