(defproject puzzle "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :plugins [[lein-exec "0.3.7"]]
  :dependencies [
  [org.clojure/clojure "1.8.0"]
  [trystan/voronoi-diagram "1.0.0"]
  [clj-kdtree "1.2.0" :exclusions [org.clojure/clojure]]]
  :main ^:skip-aot puzzle.core)
