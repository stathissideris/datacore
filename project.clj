(defproject datacore "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.9.0"]
                 [org.clojure/clojurescript "1.10.238"]

                 [mount "0.1.12"]
                 [com.taoensso/sente "1.12.0"]
                 [http-kit "2.3.0"]

                 [rum "0.11.2"]
                 [com.taoensso/sente "1.12.0"]
                 [camel-snake-kebab "0.4.0"]

                 [cljsjs/vega-embed "3.1.1-0"]
                 [cljsjs/vega "3.0.1-0"]
                 [cljsjs/vega-lite "2.0.0-0"]
                 [cljsjs/vega-tooltip "0.6.1-0"]]

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
