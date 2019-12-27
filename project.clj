(defproject com.flocktory/wet "0.2.2-SNAPSHOT"
  :description "Liquid in Clojure"
  :url "https://github.com/flocktory/wet"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.9.0"]
                 [instaparse "1.4.10"]]
  :profiles {:uberjar {:aot :all}}
  :deploy-repositories [["releases" {:url "https://clojars.org/repo"
                                     :sign-releases false}]])
