(ns mebot.client
  (:require [clojure.data.json :as json]
            [clj-http.client :as http]
            [clojure.pprint :refer [pprint]]
            [clojure.string :as str]
            [clojure.java.io :as io]
            [camel-snake-kebab.core :refer [->kebab-case]])
  (:import java.net.InetAddress
           java.net.URLEncoder
           java.net.URI
           java.util.UUID
           org.apache.tika.Tika))

(defn pthru [o] (pprint o) o)

(defmulti decode-body :content-type)
(defmethod decode-body :json [res]
  (-> res :body (json/read-str :key-fn keyword)))
(defmethod decode-body :default [res] res)

(defmulti encode-body :content-type)
(defmethod encode-body :json [req]
  (update req :body json/write-str))
(defmethod encode-body :default [req] req)

(defmulti exec-request! :method)
(defmethod exec-request! :get
  [& {:keys [url accept] :as opts}]
  (-> (pthru url)
      (http/get opts)
      (assoc :content-type accept)
      (decode-body)))
(defmethod exec-request! :post
  [& {:keys [url accept] :as opts}]
  (-> (pthru url)
      (http/post (encode-body opts))
      (assoc :content-type accept)
      (decode-body)))
(defmethod exec-request! :put
  [& {:keys [url accept] :as opts}]
  (-> (pthru url)
      (http/put (encode-body opts))
      (assoc :content-type accept)
      (decode-body)))
(defmethod exec-request! :default
  [& {:keys [method]}]
  (throw (RuntimeException. (str  "Bad method: " method))))

(defn- put-bytes!
  ([url]      (put-bytes! url {}))
  ([url opts] (-> url
                  (http/put (-> opts
                                (update :accept       (fn [_] :json))
                                (update :content-type (fn [_] :json))))
                  :body
                  (json/read-str :key-fn keyword))))

(defn- put-multipart!
  ([url]      (put-multipart! url {}))
  ([url opts] (-> url
                  (http/put (-> opts
                                (update :accept       (fn [_] :json))))
                  :body
                  (json/read-str :key-fn keyword))))

(defn- matrix-url
  [base components & {:keys [api version]
                      :or   {api "client" version 3}}]
  (letfn [(to-component [c] (URLEncoder/encode (name c) "utf-8"))]
    (str base "/_matrix/" api "/v" version "/"
         (str/join "/" (map to-component components)))))

(defn- get-token-issuer! [domain]
  (-> (exec-request! {:url    (str "https://" domain "/.well-known/matrix/openid")
                      :method :get
                      :accept :json})
      :token-issuer))

(defn- get-client-id! [domain]
  (-> (exec-request! {:url    (str "https://" domain "/.well-known/matrix/openid")
                      :method :get
                      :accept :json})
      :client-id))

(defn- get-wellknown-url! [domain]
  (-> (exec-request! {:url    (str "https://" domain "/.well-known/matrix/client")
                      :method :get
                      :accept :json})
      :m.homeserver
      :base_url))

(defn get-jwt-token! [& {:keys [domain username password]}]
  (-> (exec-request! {:url     (get-token-issuer! provider-url)
                      :method  :post
                      :accept  :json
                      :form-params {:grant_type "client_credentials"
                                    :client_id  (get-client-id! client-id)
                                    :username   username
                                    :password   password}})
      :access_token))

;;
;; Mebot Client
;;

(defn- normalize-keys [m]
  (into {} (map (fn [[k v]] [(->kebab-case k) v])) m))

(defn make-client! [domain]
  {:base-url    (get-wellknown-url! domain)})

(defn request-access! [client jwt-token]
  (let [hostname (.getCanonicalHostName (InetAddress/getLocalHost))]
    (-> (exec-request! {:url          (matrix-url (:base-url client) [:login])
                        :method       :post
                        :accept       :json
                        :content-type :json
                        :body         {:type  "org.matrix.login.jwt"
                                       :token jwt-token
                                       :initial_device_display_name hostname}})
        (normalize-keys)
        (merge client))))

(defn- set-default [default] (fn [val] (if val val default)))

(defmacro ->* [& fns]
  (let [v (gensym)]
    `(fn [~v] (-> ~v ~@fns))))

(defn get!
  ([client path] (get! client path {}))
  ([{:keys [base-url access-token]} path
    {{:keys [api version] :or {api "client" version 3}} :api :as opts}]
   (exec-request! (-> opts
                      (dissoc :api)
                      (assoc  :url     (matrix-url base-url path :api api :version version)
                              :method  :get)
                      (update :accept  (set-default :json))
                      (update :headers (->* (assoc :authorization (str "Bearer " access-token))))))))

(defn post!
  ([client path] (post! client path {}))
  ([{:keys [base-url access-token]} path
    {{:keys [api version] :or {api "client" version 3}} :api :as opts}]
   (exec-request! (-> opts
                      (assoc  :url          (matrix-url base-url path :api api :version version)
                              :method       :post)
                      (update :accept       (set-default :json))
                      (update :headers (->* (assoc :authorization (str "Bearer " access-token))))))))

(defn put!
  ([client path] (put! client path {}))
  ([{:keys [base-url access-token]} path
    {{:keys [api version] :or {api "client" version 3}} :api :as opts}]
   (exec-request! (-> opts
                      (assoc  :url          (matrix-url base-url path :api api :version version)
                              :method       :put)
                      (update :accept       (set-default :json))
                      (update :headers (->* (assoc :authorization (str "Bearer " access-token))))))))

(defn get-room-info! [client room-alias]
  (get! client [:directory :room room-alias]))

(defn list-public-rooms! [client]
  (get! client [:publicRooms]))

(defn- assoc-if-present [m k v]
  (if v (assoc m k v) m))

(defn create-private-room! [client & {:keys [name alias topic invitees]
                                      :or   {invitees []}}]
  (post! client [:createRoom]
         {:content-type :json
          :body         (-> {:preset "private_chat"}
                            (assoc-if-present :name   name)
                            (assoc-if-present :alias  alias)
                            (assoc-if-present :topic  topic)
                            (assoc            :invite invitees))}))

(defn get-room-members! [client room-id]
  (get! client [:rooms room-id :members]))

(defn is-message? [msg]
  (-> msg :type (= "m.room.message")))

(defn mentions? [user msg]
  (letfn [(matches-user? [u] (= u user))]
    (some->> msg :content :m.mentions :user_ids (some matches-user?))))

(defn get-room-events! [client room-id]
  (get! client [:rooms room-id :messages]
        {:query-params {:dir "b"}}))

(defn get-room-messages! [client room-id]
  (->> (get-room-events! client room-id)
       :chunk
       (filter is-message?)))

(defn get-room-mentions! [client room-id user]
  (filter (partial mentions? user)
          (get-room-messages! client room-id)))

(defn send-message! [client room-id msg]
  (let [txn-id (str (UUID/randomUUID))]
    (put! client [:rooms room-id :send "m.room.message" txn-id]
          {:content-type :json
           :accept       :json
           :body         {:msgtype "m.text"
                          :body    msg}})))

(defn invite-user! [client room-id user-id]
  (post! client [:rooms room-id :invite]
         {:body         {:user_id user-id}
          :accept       :json
          :content-type :json}))

(defn- detect-mime-type [is]
  (let [tika (Tika.)]
    (.detect tika is)))

(defn send-image! [client room-id img-data filename]
  (let [media-uri   (-> (post! client [:create]
                               {:api {:api "media" :version 1}})
                        :content_uri
                        (URI.))
        media-id    (-> media-uri (.getPath) (io/file) (.getName))
        server-name (-> media-uri (.getHost))]
    (put! client [:upload server-name media-id]
          {:headers      {:content-type (detect-mime-type img-data)}
           :query-params {:filename filename}
           :api          {:api "media" :version 3}
           :body         img-data})
    (let [txn-id (str (UUID/randomUUID))]
      (put! client [:rooms room-id :send "m.room.message" txn-id]
            {:body {:msgtype "m.image"
                    :body    filename
                    :url     (str media-uri)}
             :accept       :json
             :content-type :json}))))
