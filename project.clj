(defproject accent "0.1.10"
  :description "WebGL utility belt"
  :url "http://github.com/skrat/accent"
  :author "skrat"
  :license {:name "MIT License"
            :url "http://www.opensource.org/licenses/mit-license.php"}

  :dependencies [[org.clojure/clojure "1.8.0"]
                 [org.clojure/clojurescript "1.8.40"]
                 [org.clojure/core.async "0.2.374"]]

  :plugins [[lein-cljsbuild "1.0.3"]]

  :source-paths ["src"])
