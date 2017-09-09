(defproject datacore "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :repositories [["bintray" "http://dl.bintray.com/jerady/maven"]]
  :dependencies [[org.clojure/clojure "1.9.0-alpha14"]
                 [org.clojure/math.combinatorics "0.1.4"]
                 [org.clojure/data.priority-map "0.0.7"]
                 [org.clojure/data.csv "0.1.3"]
                 [org.clojure/core.match "0.3.0-alpha4"]
                 [me.raynes/fs "1.4.6"]
                 [org.clojure/core.async "0.3.443"]
                 [hawk "0.2.11"]
                 [hiccup "1.0.5"]

                 ;;see https://bintray.com/jerady/maven/FontAwesomeFX/view#
                 [de.jensd/fontawesomefx-commons "8.15"]
                 [de.jensd/fontawesomefx-controls "8.15"]
                 [de.jensd/fontawesomefx-controls "8.15"]
                 [de.jensd/fontawesomefx-fontawesome "4.7.0-5"]
                 [de.jensd/fontawesomefx-materialicons "2.2.0-5"]]
  :resource-paths ["resources"]
  :main datacore.main
  :profiles {:dev {:source-paths   ["dev"]
                   :resource-paths ["lib/scenicView.jar"]
                   :dependencies   [[org.clojure/tools.namespace "0.2.11"]
                                    [org.clojure/test.check "0.9.0"]]
                   :jvm-opts       ["-agentlib:jdwp=transport=dt_socket,server=y,address=8700,suspend=n"
                                    "-XX:-OmitStackTraceInFastThrow"]}})
