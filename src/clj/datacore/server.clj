(ns datacore.server
  (:require [clojure.java.io :as io]
            [taoensso.timbre :as log]
            [mount.core :refer [defstate] :as mount]
            [ring.util.response :refer [content-type]]
            [ring.middleware.gzip :refer [wrap-gzip]]
            [ring.middleware.cljsjs :refer [wrap-cljsjs]]
            [org.httpkit.server :as http-kit]
            [compojure.core :as comp :refer [defroutes GET POST]]
            [compojure.route :as route]
            [taoensso.sente :as sente]
            [taoensso.sente.server-adapters.http-kit :as sente.http-kit]))


(defstate sente :start (sente/make-channel-socket!
                        (sente.http-kit/get-sch-adapter) {}))

(defroutes routes
  (GET "/status" req (-> {:status 200 :body "ok"}
                         (content-type "text/html")))
  (GET "/" req (content-type {:status 200
                              :body   (io/input-stream (io/resource "public/index.html"))} "text/html"))
  (GET "/chsk" req ((:ring-ajax-get-or-ws-handshake sente) req))
  (POST "/chsk" req ((:ring-ajax-post sente) req))
  (route/not-found "<h1>404</h1>"))

(def app
  (-> routes
      wrap-cljsjs
      wrap-gzip))

(defstate http-server
  :start (http-kit/run-server app {})
  :stop  (http-server))
