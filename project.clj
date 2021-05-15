(defproject aeons-end "0.1.0-SNAPSHOT"
  :description "Aeon's End"
  :url "http://example.com/FIXME"
  :dependencies [[org.clojure/clojure "1.10.1"]
                 [org.clojure/clojurescript "1.10.764"]
                 [reagent "0.8.1"]]

  :plugins [[lein-cljsbuild "1.1.7"]
            [lein-figwheel "0.5.20"]]

  :clean-targets ^{:protect false} [:target-path
                                    [:cljsbuild :builds :app :compiler :output-dir]
                                    [:cljsbuild :builds :app :compiler :output-to]]

  :resource-paths ["public"]

  :figwheel {:css-dirs ["public/css"]}

  :cljsbuild {:builds {:app     {:source-paths ["src" "env/dev/cljs"]
                                 :compiler     {:main          "aeons-end.dev"
                                                :output-to     "public/js/app.js"
                                                :output-dir    "public/js/out"
                                                :asset-path    "js/out"
                                                :source-map    true
                                                :optimizations :none
                                                :pretty-print  true}
                                 :figwheel     {:on-jsload "aeons-end.core/mount-root"
                                                :open-urls ["http://localhost:63342/aeons-end/index.html" #_"http://localhost:3449/index.html"]}}
                       :release {:source-paths ["src" "env/prod/cljs"]
                                 :compiler     {:output-to     "public/js/app.js"
                                                :output-dir    "public/js/release"
                                                :asset-path    "js/out"
                                                :optimizations :advanced
                                                :pretty-print  false}}}}

  :aliases {"package" ["do" "clean" ["cljsbuild" "once" "release"]]}

  :profiles {:dev {:source-paths ["src" "env/dev/clj"]
                   :dependencies [[org.clojure/clojurescript "1.10.773"]
                                  [binaryage/devtools "0.9.10"]
                                  #_[com.bhauman/figwheel-main "0.2.13"]
                                  ;; optional but recommended
                                  [com.bhauman/rebel-readline-cljs "0.1.4"]
                                  [figwheel-sidecar "0.5.20"]
                                  [cider/piggieback "0.4.0" :exclusions [org.clojure/clojurescript]]]
                   :repl-options {:timeout          1200000
                                  :nrepl-middleware [cider.piggieback/wrap-cljs-repl]}}})
