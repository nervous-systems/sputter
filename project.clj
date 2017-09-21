(defproject io.nervous/sputter "0.1.0-SNAPSHOT"
  :url          "https://github.com/nervous-systems/sputter"
  :license      {:name "Unlicense" :url "http://unlicense.org/UNLICENSE"}
  :scm          {:name "git" :url "https://github.com/nervous-systems/sputter"}
  :dependencies [[org.clojure/clojure             "1.9.0-alpha16"]
                 [pandect                         "0.6.0"]
                 [org.bouncycastle/bcprov-jdk15on "1.54"]]
  :profiles     {:dev {:dependencies [[cheshire "5.8.0"]]}})
