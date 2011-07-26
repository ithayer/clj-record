(defproject clj-record "1.0.3-SNAPSHOT"
  :description "A pseudo-port of ActiveRecord to the Clojure programming language"
  :url "http://github.com/ithayer/clj-record"
  :dependencies [[org.clojure/clojure "1.2.1"]
                 [org.clojure/clojure-contrib "1.2.0"]
                 [org.clojure/java.jdbc       "0.0.5"]
                 [postgresql "8.4-701.jdbc4"]]
  :dev-dependencies [[org.apache.derby/derby "10.5.3.0_1"]])


(ns leiningen.reset-db
  (:require leiningen.compile))

(defn reset-db [project]
  (leiningen.compile/eval-in-project project
    '(do
      (require 'clj-record.test-helper)
      (clj-record.test-helper/reset-db))))
