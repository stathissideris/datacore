(defproject datacore "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[halgari/fn-fx "0.3.0-SNAPSHOT"]
                 [org.clojure/data.csv "0.1.3"]
                 [org.clojure/clojure "1.8.0"]]
  :profiles {:dev {:source-paths ["dev"]
                   :dependencies [[org.clojure/tools.namespace "0.2.11"]
                                  [com.github.javaparser/javaparser-core "2.5.1"]
                                  [org.jboss.forge.roaster/roaster-api "2.19.0.Final"]
                                  [org.jboss.forge.roaster/roaster-jdt "2.19.0.Final"]]}})
