(defproject foobar "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [org.clojure/jvm.tools.analyzer "0.4.2"]
                 [javassist/javassist "3.12.1.GA"]
                 [leiningen-core "2.2.0"]
                 [classlojure "0.6.6"]]
  :aot [foobar.classloader]
  :main foobar.core)
