(defproject accent "0.1.4"
  :description "WebGL utility belt"
  :url "http://github.com/skrat/accent"
  :author "skrat"
  :license {:name "MIT License"
            :url "http://www.opensource.org/licenses/mit-license.php"}

  :dependencies [[org.clojure/clojure "1.6.0"]
                 [org.clojure/clojurescript "0.0-2227"]
                 [org.clojure/core.async "0.1.301.0-deb34a-alpha"]]

  :plugins [[lein-cljsbuild "1.0.3"]]

  :source-paths ["src"])
