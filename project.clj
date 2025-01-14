;;    Copyright 2010 Adam C. Foltzer
;;
;;    Licensed under the Apache License, Version 2.0 (the "License");
;;    you may not use this file except in compliance with the License.
;;    You may obtain a copy of the License at
;;
;;      http://www.apache.org/licenses/LICENSE-2.0
;;
;;    Unless required by applicable law or agreed to in writing, software
;;    distributed under the License is distributed on an "AS IS" BASIS
;;    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;;    See the License for the specific language governing permissions and
;;    limitations under the License.

(defproject cljcr
  "1.0.0-SNAPSHOT"
  :description "cljcr is a Clojure library for accessing the JSR-283 Java Content Repository API in an idiomatic, functional way."
  :dependencies     [[org.clojure/clojure "1.3.0"]
                     [org.clojure.contrib/except "1.3.0-SNAPSHOT"]
                     [javax.jcr/jcr "2.0"]]
  :dev-dependencies [[org.apache.jackrabbit/jackrabbit-core "2.2.9"]
                     [org.apache.jackrabbit/jackrabbit-jcr-rmi "2.2.8"]
                     [org.slf4j/slf4j-log4j12 "1.5.8"]])
