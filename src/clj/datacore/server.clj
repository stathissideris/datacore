(ns datacore.server
  (:require [clojure.java.io :as io]
            [taoensso.timbre :as log]
            [mount.core :refer [defstate] :as mount]
            [ring.util.response :refer [content-type]]
            [ring.middleware.params :refer [wrap-params]]
            [ring.middleware.keyword-params :refer [wrap-keyword-params]]
            [ring.middleware.resource :refer [wrap-resource]]
            [ring.middleware.content-type :refer [wrap-content-type]]
            [ring.middleware.not-modified :refer [wrap-not-modified]]
            [ring.middleware.gzip :refer [wrap-gzip]]
            [ring.middleware.cljsjs :refer [wrap-cljsjs]]
            [org.httpkit.server :as http-kit]
            [compojure.core :as comp :refer [defroutes GET POST]]
            [compojure.route :as route]
            [taoensso.sente :as sente]
            [taoensso.sente.server-adapters.http-kit :as sente.http-kit]
            [clojure.core.async :refer [go-loop <!]]))


(defstate sente :start (sente/make-channel-socket!
                        (sente.http-kit/get-sch-adapter)
                        {:user-id-fn (fn [req] "stathis")}))

(defroutes routes
  (GET "/status" req (-> {:status 200 :body "ok"}
                         (content-type "text/html")))
  (GET "/" req (content-type {:status 200
                              :body   (io/input-stream (io/resource "public/index.html"))} "text/html"))
  (GET "/chsk" req ((:ajax-get-or-ws-handshake-fn sente) req))
  (POST "/chsk" req ((:ajax-post-fn sente) req))
  (route/not-found "<h1>404</h1>"))

(def app
  (-> routes
      wrap-keyword-params
      wrap-params
      (wrap-resource "public")
      wrap-content-type
      wrap-not-modified
      wrap-cljsjs
      wrap-gzip))

(defstate http-server
  :start (http-kit/run-server app {})
  :stop  (http-server))

(go-loop []
  (let [{:as msg :keys [event]} (<! (:ch-recv sente))]
    (println (pr-str event))
    (when (= :mouse/move (first event))
      ((:send-fn sente) "stathis" event))
    (recur)))

;; to send events:
;; ((:send-fn sente) "stathis" [:my/foo {:text "hi!"}])
