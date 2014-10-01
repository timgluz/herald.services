(ns herald.services.veye.core
  (:require [schema.core :as s]
            [schema.macros :as sm]
            [blancas.morph.monads :refer [either left right]]
            [clojure.string :as string]
            [taoensso.timbre :as log]
            [herald.services.impl :refer [IRPC_Client IAuthorizer]]
            [herald.services.utils :refer [do-request build-url build-request]]
            [herald.services.schemas :as schemas
                                     :refer [response-walker matches-schema?
                                             ->SRResponse ->SRError
                                             ->SRPagedEntity ->SREntity]])
  (:import [blancas.morph.monads Either]))

;;-- Client implementation
(sm/defrecord VeyeClient
  [api-url  :- s/Str
   auth     :- schemas/Auth
   client-opts :- schemas/ClientOptions]
  IAuthorizer
  (append-auth [this request-dt]
    (assoc-in request-dt
              [:query-params :api_key]
              (get-in this [:auth :secret])))
  IRPC_Client
  (rpc-call [this method path]
    (.rpc-call this method path {} {}))
  (rpc-call [this method path query-params]
    (.rpc-call this method path query-params {}))
  (rpc-call [this method path query-params extra-client-opts]
    (let [request-dt (build-request this method path query-params extra-client-opts)]
      (either [resp (do-request (.append-auth this request-dt))]
        (do
          (log/error "VeyeClient:rpc-call failed: " path "\n " resp)
          (left (->SRError (:status resp)
                           (str "VeyeClient cant access resource: " path)
                           resp)))
        (if (< 199 (:status resp) 300)
          (right (->SRResponse (:status resp) (:headers resp) (:body resp)))
          (left (->SRError (:status resp)
                           (str "VeyeClient:rpc-call request failed")
                           resp)))))))



;;-- COERCERS
(def VeyeUser {:fullname s/Str
               :username s/Str
               :admin s/Bool
               s/Keyword s/Any})

(defmulti coerce-user-item
  (fn [block]
    (if (matches-schema? VeyeUser block)
      :entity
      :default)))

(defmethod coerce-user-item :entity [block]
  {:name (:fullname block)
   :username (:username block)
   :type (if (:admin block)
           "admin"
           "user")})

(defmethod coerce-user-item :default [block]
  block)

;;-- Search coercers
(defmulti sub-coerce-search
  (fn [block]
    (when (and (vector? block)
               (= 2 (count block)))
      (first block))))

(defmethod sub-coerce-search :results [[item-key items]]
  [:results (vec
              (map (fn [item]
                        {:name (-> item :name str string/lower-case)
                        :language (-> item :language str string/lower-case)
                        :product-id (-> item :prod_key str)
                        :version (-> item :version str)})
                    items))])

(defmethod sub-coerce-search :paging [[item-key item]]
  [item-key {:current     (:current_page item)
             :per-page    (:per_page item)
             :total       (:total_pages item)
             :total-items (:total_entries item)}])

(defmethod sub-coerce-search :query [[item-key item]]
  [item-key {:query     (:q item)
             :languages (:lang item)}])

(defmethod sub-coerce-search :default [item]
  item)


(def VeyeSearchItem {:name s/Str
                     :language s/Str
                     :prod_key s/Str
                     :version s/Str
                     :prod_type s/Str})

(def VeyeDependency {:name s/Str
                     :prod_key s/Str
                     :group_id (s/either s/Str (s/pred nil?))
                     :artifact_id (s/either s/Str (s/pred nil?))
                     (s/optional-key :license) (s/either s/Str (s/pred nil?))
                     :version_current s/Str
                     :version_requested s/Str
                     :comparator s/Str
                     :unknown s/Bool
                     :outdated s/Bool
                     :stable s/Bool})

(def VeyeProjectItem {:id s/Str
                      :project_key s/Str
                      :name s/Str
                      :project_type s/Str
                      :public s/Bool
                      :private_scm s/Bool
                      :period s/Str
                      :source s/Str
                      :dep_number s/Int
                      :out_number s/Int
                      :created_at s/Str
                      :updated_at s/Str
                      (s/optional-key :dependencies) [VeyeDependency]
                      s/Keyword s/Any})


(def VeyeEntityResponse (s/either VeyeProjectItem))

(def VeyeListResponse [(s/either VeyeProjectItem)])

(def VeyePagedResponse {:results [(s/either VeyeSearchItem)]
                        (s/optional-key :query) {:q s/Str
                                                 (s/optional-key :lang) [s/Str]
                                                 (s/optional-key :g) s/Any}
                        (s/optional-key :paging) {:current_page s/Int
                                                  :per_page s/Int
                                                  :total_pages s/Int
                                                  :total_entries s/Int}})
;;-- PROJECT coercers
(defmulti coerce-project-item
  (fn [block]
    (cond
      (and (coll? block)
           (= 2 (count block))
           (= :dependencies (first block))) :dependencies
      (and (map? block)
           ((comp empty? s/check) VeyeProjectItem block)) :entity
      :else :default
      )))

(defmethod coerce-project-item :default [item]
  item)

;;TODO: is coercion required here?
(defmethod coerce-project-item :dependencies [[item-key deps]]
  [item-key deps])

(defmethod coerce-project-item :entity [proj]
  {:veye_id     (:id proj)
   :project_key (:project_key proj)
   :name        (:name proj)
   :project_type (:project_type proj)
   :public      (:public proj)
   :period      (:period proj)
   :source      (:source proj)
   :dep_number  (:dep_number proj)
   :out_number  (:out_number proj)
   :dependencies (get proj :dependencies [])})

;;-- RESPONSE coercers
(defmulti coerce-response (fn [type _] type))

(defmethod coerce-response ::search [_ response]
   ((response-walker VeyePagedResponse sub-coerce-search)
     (get response :body {})))

(defmethod coerce-response ::projects [_ response]
  ((response-walker VeyeListResponse coerce-project-item)
    (get response :body [])))

(defmethod coerce-response ::project [_ response]
  ((response-walker VeyeEntityResponse coerce-project-item)
   (get response :body {})))

(defmethod coerce-response ::user [_ response]
  ((response-walker VeyeUser coerce-user-item)
   (get response :body {})))

(defmethod coerce-response ::raw [_ response]
  (get response :body {}))

(defmethod coerce-response :default [type response]
  (log/error "Veye: Not supported coercer: " type))

;;-- API functions
(sm/defn get-current-user :- Either
  "Fetches user profile."
  [client :- VeyeClient]
  (either [resp (.rpc-call client :get "me")]
    (left resp)
    (if-let [coerced-dt (coerce-response ::user resp)]
      (right (->SREntity coerced-dt))
      (left (->SRError 503 "Coercing error - get-current-user" resp)))))

(sm/defn search :- Either
  "Search packages on VersionEye."
  [client      :- VeyeClient
   search-term :- s/Str
   search-args :- s/Any]
  (either [resp (.rpc-call client :get ["products/search" search-term])]
    (left (->SRError (:status resp) "Failed search" (:body resp)))
    (if-let [coerced-dt (coerce-response ::search resp)]
      (right (->SRPagedEntity (:results coerced-dt) (:paging coerced-dt)))
      (left (->SRError 503 "Coercing error - ::search schema" resp)))))

(sm/defn get-projects :- Either
  "get linked projects on VersionEye"
  [client :- VeyeClient]
  (either [resp (.rpc-call client :get "projects")]
    (left (->SRError (:status resp) "Error while requesting projects list" (:body resp)))
    (if-let [coerced-dt (coerce-response ::projects resp)]
      (right (->SREntity coerced-dt))
      (left (->SRError 503 "Coercing error - ::projects schema" resp)))))

(sm/defn get-project :- Either
  "get a project related meta-data and a state of dependencies"
  [client :- VeyeClient
   project-key :- s/Str]
  (either [resp (.rpc-call client :get ["projects" project-key])]
    (left (->SRError (:status resp) "No project info" (:body resp)))
    (if-let [coerced-dt (coerce-response ::project resp)]
      (right (->SREntity coerced-dt))
      (left (->SRError 503 "Coercing error - get::project schema" resp)))))

(sm/defn spit->temp
  "Spits file-content into temporary file;"
  [file-name :- s/Str
   file-content :- s/Str]
  (let [temp-filename (str "_" file-name)
        temp-file (java.io.File/createTempFile "tmp_" temp-filename)]
    (spit temp-file file-content)
    temp-file))

(sm/defn create-project :- Either
  "creates new veye project by passing a content of projectfile as string
  NB!: it's valid only for a project files, but not for binary files."
  [client :- VeyeClient
   file-name :- s/Str
   file-content :- s/Str]
  (if (empty? file-content)
    (left (->SRError 404 (str "File content is empty") ""))
    (let [temp-file (spit->temp file-name file-content)]
      ;; NB! we need temporal file, because veye.API doesnt
      ;; work when :content is plain string
      (either [resp (.rpc-call client
                              :post ["projects"] {}
                              {:multipart [{:name "upload"
                                            :content temp-file
                                            :filename file-name}]})]
        (left (->SRError 503 (str "Cant create new project for: " file-name) resp))
        (if-let [coerced-dt (coerce-response ::project resp)]
          (right (->SREntity coerced-dt))
          (left (->SRError 503 "Coercing error - create::project schema" resp)))))))


(sm/defn delete-project :- Either
  "deletes existing project from VersionEye."
  [client :- VeyeClient
   project-key :- s/Str]
  (either [resp (.rpc-call client :delete ["projects" project-key])]
    (left (->SRError (:status resp)
                    (str "Error while tried to delete project `" project-key "`.")
                    resp))
    (right (->SREntity (get-in resp [:body :success] false)))))


(sm/defn update-project :- Either
  "updates existing project with the file-content"
  [client :- VeyeClient
   project-key :- s/Str
   file-name :- s/Str
   file-content :- s/Str]
  (if (empty? file-content)
    (left (->SRError 404 "File content is missing or empty." ""))
    (let [temp-file (spit->temp file-name file-content)]
      (either [resp (.rpc-call client
                              :post ["projects" project-key] {}
                              {:multipart [{:name "project_key"
                                            :content project-key}
                                           {:name "project_file"
                                            :content temp-file
                                            :filename file-name}]})]
        (left (->SRError 503 (str "Cant update project: " project-key) file-name))
        (if-let [coerced-dt (coerce-response ::project resp)]
          (right (->SREntity coerced-dt))
          (left (->SRError 503 "Coercing error - update::project schema" resp)))))))
