;; Version number: <circle-ci-bond-base>.<forked-version>
;;                                 0.4.0.0.1.0
;; This is forked off circleci/bond 0.4.0
(defproject helpshift/bond "0.4.0.0.1.0"
  :github "https://github.com/helpshift/bond"
  :description "Spying library for testing"
  :url "https://github.com/helpshift/bond"
  :deploy-repositories {"releases" {:url "https://repo.clojars.org" :creds :gpg}}
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies []
  :plugins [[lein-test-out "0.3.1" :exclusions [org.clojure/clojure]]
            [lein-cljsbuild "1.1.3"]
            [lein-cloverage "1.0.9"]]
  :cljsbuild {:builds [{:source-paths ["src" "test"]
                        :compiler {:output-dir "resources/public/js/out"
                                   :output-to "resources/public/js/test-bond.js"
                                   :optimizations :whitespace
                                   :pretty-print true}}]
              :test-commands {"unit" ["node_modules/phantomjs-prebuilt/bin/phantomjs"
                                      "resources/test/phantom/runner.js"
                                      "resources/test/test.html"]}}
  :profiles {:dev {:dependencies [[org.clojure/clojure "1.7.0"]
                                  [org.clojure/clojurescript "1.7.228"]
                                  [com.cemerick/clojurescript.test "0.3.0"]]}})
