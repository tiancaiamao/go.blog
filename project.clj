(defproject blog "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Creative Commons Attribution Share-Alike 4.0 International"
            :url  "https://creativecommons.org"}
  :dependencies [[org.clojure/clojure "1.10.0"]
                 [org.clojure/data.json "0.2.6"]
                 [ring "1.7.1"]
                 [compojure "1.6.1"]
                 [hiccup "1.0.5"]
                 [markdown-to-hiccup "0.6.2"]]
  :main blog.core
  :profiles {:dev {:main blog.core/-dev-main}}
  )
