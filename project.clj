(defproject datacore "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.9.0"]
                 [org.clojure/clojurescript "1.10.238"]

                 ;;state
                 [mount "0.1.12"]

                 ;;utils
                 [camel-snake-kebab "0.4.0"]
                 [expound "0.5.0"]

                 ;;server
                 [ring/ring-core "1.6.3"]
                 [bk/ring-gzip "0.3.0"]
                 [ring-cljsjs "0.1.0"]
                 [compojure "1.6.1"]
                 [http-kit "2.3.0"]
                 [com.taoensso/sente "1.12.0"]
                 [org.clojure/core.async "0.4.474"]

                 ;;client
                 [rum "0.11.2"]

                 ;;viz
                 [cljsjs/vega-embed "3.1.1-0"]
                 [cljsjs/vega "3.0.1-0"]
                 [cljsjs/vega-lite "2.0.0-0"]
                 [cljsjs/vega-tooltip "0.6.1-0"]]

  :profiles {:dev {:dependencies [[org.clojure/tools.namespace "0.2.11"]]
                   :source-paths ["dev"]}}

  :jvm-opts ^:replace ["-Xmx1g" "-server"]

  :plugins [[lein-cljsbuild "1.1.7"
             :exclusions [org.clojure/clojure]]
            [lein-figwheel "0.5.15"]]

  :clean-targets ^{:protect false} ["resources/public/js/out"
                                    "resources/public/js/datacore.js"
                                    :target-path]

  :source-paths ["src/clj"
                 "src/cljs"]

  :cljsbuild {:builds [{:id "datacore"
                        :source-paths ["src/cljs"]
                        :figwheel true
                        :compiler {:main datacore.main
                                   :asset-path "js/out"
                                   :output-to "resources/public/js/datacore.js"
                                   :output-dir "resources/public/js/out"
                                   :source-map-timestamp true}}]}

  :figwheel { :css-dirs ["resources/public/css"]
              :open-file-command "emacsclient"})
