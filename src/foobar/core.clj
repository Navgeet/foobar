(ns foobar.core
  (:require [foobar.analyze :as ana]
            [clojure.tools.analyzer :as analyzer]
            [leiningen.core.project :as pro]
            [classlojure.core :as cl])
  (:use [clojure.java.io :only [as-url]])
  (:import [java.net URL]
           [foobar.classloader CodeqClassLoader]))

(defn codeq-classloader [& urls]
  (let [urls (into-array URL (map as-url (flatten urls)))
        ^CodeqClassLoader cq-cl (CodeqClassLoader. urls cl/ext-classloader)]
    (.loadClass cq-cl "clojure.lang.RT")
    (cl/eval-in* cq-cl '(require 'clojure.main))
    (cl/eval-in* cq-cl '(require 'clojure.tools.analyzer))
    cq-cl))

(def cq-cl (codeq-classloader "file:clojure-1.4.0.jar"
                              "file:jvm.tools.analyzer-0.4.2.jar"))

(defn eval-in* [cl form & objects]
  (let [print-read-eval (fn [form]
                          (->> (pr-str form)
                               (cl/invoke-in cl clojure.lang.RT/readString [String])
                               (cl/invoke-in cl clojure.lang.Compiler/eval [Object])))]
    (cl/with-classloader cl
      (let [result-or-fn (print-read-eval form)
            result (if (seq objects)
                     (-> (class result-or-fn)
                         (.getMethod "invoke"
                                     (into-array (repeat (count objects) Object)))
                         (.invoke result-or-fn (to-array objects)))
                     result-or-fn)]
        result))))

(defn analyze-form-in-cl [form loader]
  (let [ast (eval-in* loader `(clojure.tools.analyzer/analyze-form
                               '~form {:java-obj true}))]
    ast))
