(ns mebot.client
  (:require [clojure.data.json :as json]
            [clj-http.client :as http]
            [clojure.pprint :refer [pprint]]
            [clojure.string :as str]
            [clojure.java.io :as io]
            [clojure.core.async :refer [>! <! chan go-loop timeout pipeline]]
            [camel-snake-kebab.core :refer [->kebab-case]]
            [slingshot.slingshot :refer [try+ throw+]])
  (:import java.net.InetAddress
           java.net.URLEncoder
           java.net.URI
           java.util.UUID
           org.apache.tika.Tika
           clojure.lang.ExceptionInfo))

(defn pthru [o]
  (pprint o)
  (flush)
  o)

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
  (-> url
      (http/get opts)
      (assoc :content-type accept)
      (decode-body)))
(defmethod exec-request! :post
  [& {:keys [url accept] :as opts}]
  (-> url
      (http/post (encode-body opts))
      (assoc :content-type accept)
      (decode-body)))
(defmethod exec-request! :put
  [& {:keys [url accept] :as opts}]
  (-> url
      (http/put (encode-body opts))
      (assoc :content-type accept)
      (decode-body)))
(defmethod exec-request! :default
  [& {:keys [method]}]
  (throw (RuntimeException. (str  "Bad method: " method))))

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
  (-> (exec-request! {:url     (str (get-token-issuer! domain) "/")
                      :method  :post
                      :accept  :json
                      :form-params {:grant_type "client_credentials"
                                    :client_id  (get-client-id! domain)
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

(defn- make-room [client room-id]
  { :client client :room-id room-id })

(defn create-private-room! [client & {:keys [name alias topic invitees]
                                      :or   {invitees []}}]
  (->> (post! client [:createRoom]
              {:content-type :json
               :body         (-> {:preset "private_chat"}
                                 (assoc-if-present :name   name)
                                 (assoc-if-present :alias  alias)
                                 (assoc-if-present :topic  topic)
                                 (assoc            :invite invitees))})
       :room_id
       (make-room client)))

(defn join-public-room! [client & {:keys [alias]}]
  (try
    (->> (post! client [:join alias]
                {:content-type :json
                 :body         {:reason "mabel client joining"}})
         (pthru)
         :room_id
         (make-room client))
    (catch ExceptionInfo e
      (let [info (ex-info e)]
        (if (= (:status info) 403)
          (throw+ {:type ::forbidden :room-alias alias} e))))))

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

(defn get-current-event-stamp! [client]
  (-> (get! client [:sync]) :next_batch))

(defn get-room-messages! [client room-id]
  (->> (get! client [:rooms room-id :messages]
             {:query-params {:dir "b"}})
       :chunk
       (filter is-message?)))

(defn get-room-events-from! [client room-id from]
  (get! client [:rooms room-id :messages]
        {:query-params {:dir "b" :from from}}))

(defn room-event-channel!
  ([room] (room-event-channel! room {}))
  ([{:keys [room-id client]}
    {:keys [buffer poll-freq]
     :or   {buffer    5
            poll-freq 5}}]
   (let [evt-chan (chan buffer)
         seen-messages (atom #{})]
     (go-loop [response (get-room-events-from! client room-id
                                               (get-current-event-stamp! client))]
       (doseq [{:keys [event_id] :as evt} (:chunk response)]
         (when (not (contains? @seen-messages event_id))
           (swap! seen-messages conj event_id)
           (>! evt-chan evt)))
       (<! (timeout (* poll-freq 1000)))
       (recur (get-room-events-from! client room-id (:end response))))
     evt-chan)))

(defn- parallelism []
  (-> (Runtime/getRuntime)
      (.availableProcessors)
      (+ 1)))

(defn pipe [in xf]
  (let [out (chan)]
    (pipeline (parallelism) out xf in)
    out))

(defn room-message-channel! [& args]
  (pipe (apply room-event-channel! args)
        (filter is-message?)))

(defn room-self-mention-channel! [{{:keys [user-id]} :client :as room} & rest]
  (pipe (apply room-message-channel! (cons room rest))
        (filter (partial mentions? user-id))))

(defn get-room-mentions! [client room-id user]
  (filter (partial mentions? user)
          (get-room-messages! client room-id)))

(defn get-room-self-mentions! [{:keys [user-id] :as client} room-id]
  (get-room-mentions! client room-id user-id))

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

(defn room-message! [{:keys [client room-id]} msg]
  (send-message! client room-id msg))

(defn room-image! [{:keys [client room-id]} bytes filename]
  (send-image! client room-id bytes filename))

(defn room-mentions! [{:keys [client room-id]} user]
  (get-room-mentions! client room-id user))

(defn room-self-mentions! [{:keys [room-id client]}]
  (get-room-self-mentions! client room-id))
