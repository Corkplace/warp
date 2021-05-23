(defproject place.cork/warp "0.1.0-SNAPSHOT"
  :description "Simple Parsing Library"
  :url "https://gitlab.com/cork.place/cork.warp"
  :license {:name    "GPL-3.0"
            :comment "GNU General Public License v3.0"
            :url     "https://choosealicense.com/licenses/gpl-3.0"
            :year    2021
            :key     "gpl-3.0"}
  :dependencies [[org.clojure/clojure "1.10.3"]]
  :source-paths ["src"]
  :resource-paths ["resources"]
  :test-paths ["test"]
  :deploy-repositories [["releases" :clojars]
                        ["snapshots" :clojars]]
  :plugins [[lein-license "1.0.0"]
            [lein-pprint "1.3.2"]])
