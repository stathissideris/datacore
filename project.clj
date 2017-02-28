(defproject datacore "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.9.0-alpha14"]
                 [org.clojure/math.combinatorics "0.1.4"]
                 [hawk "0.2.11"]
                 ;;[org.clojure/tools.analyzer "0.6.9"]
                 [org.clojure/tools.analyzer.jvm "0.7.0"]
                 [org.clojure/data.csv "0.1.3"]]
  :resource-paths ["resources"]
  :profiles {:dev {:source-paths ["dev"]
                   :dependencies [[org.clojure/tools.namespace "0.2.11"]]
                   :jvm-opts ["-agentlib:jdwp=transport=dt_socket,server=y,address=8000,suspend=n"]}})
