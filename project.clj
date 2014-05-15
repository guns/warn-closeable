(defproject com.sungpae/warn-closeable "0.1.0-SNAPSHOT"
  :description "Warn on potentially unclosed (Auto)Closeable resources."
  :url "https://github.com/guns/warn-closeable"
  :license {:name "MIT License"
            :url "http://opensource.org/licenses/MIT"}
  :dependencies [[org.clojure/tools.namespace "0.2.4"]
                 [org.clojure/tools.analyzer "0.1.0-beta13"]
                 [org.clojure/tools.analyzer.jvm "0.1.0-beta13"]]
  :profiles {:dev {:dependencies [[org.clojure/clojure "1.4.0"]]}})
