(defproject hmm-clj "0.0.1"
  :description "Hidden markov model implemented in clojure"
  :url "https://github.com/syou6162/hmm-clj"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.4.0"]
                 [info.yasuhisay/clj-utils "0.1.0-SNAPSHOT"]
                 [fobos_multiclass_clj "0.1.4"]]
  :jvm-opts ["-Xmx8g" "-server" "-Dfile.encoding=UTF-8"]
  :main hmm-clj.core)
