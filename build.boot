#!/usr/bin/env boot

(def +version+ "1.0.0-alpha1-SNAPSHOT")

(set-env!
  :project      'fulabdsl
  :version      +version+
  :dependencies '[
                  [ns-tracker "0.2.2"]
                  [adzerk/boot-reload        "0.2.6"]
                  [fipp "0.5.2"]
                  [regexpforobj "1.0.0-alpha2-SNAPSHOT"]
                  [org.clojure/core.typed "0.3.18"]
                  [adzerk/bootlaces          "0.1.10"]
                  ]
  :source-paths    #{"src"}
  )


(require '[adzerk.bootlaces :refer :all])


(bootlaces! +version+)

(task-options!
  pom  {:project     'fulab.zarnidict/fulabdsl
        :version     +version+
        :description "Parser for natural language dictionaries in FULabDSL format."
        ;:url         ""
        ;:scm         {:url ""}
        :license     {"Eclipse Public License" "http://www.eclipse.org/legal/epl-v10.html"}})


(use '[ns-tracker.core :only [ns-tracker]])
(require '[adzerk.boot-reload :refer [reload]])
(require '[fulab.zarnidict.fulabdsl.core])

(deftask wrap-reload
  "Reload namespaces of modified files before the request is passed to the
  supplied handler.
  Accepts the following options:
 :dirs - A list of directories that contain the source files.
          Defaults to [\"src\"]."
  ;{:arglists '([handler] [handler options])}
  ;[handler & [options]]
  [d dirs [str] "Dirs"]
  (let [source-dirs (or dirs ["src"])
        modified-namespaces (ns-tracker source-dirs)]
    (fn [handler]
      (fn [request]
      (doseq [ns-sym (modified-namespaces)]
        (require ns-sym :reload))
      (handler request)))))

(deftask main
  []
  (with-pre-wrap fileset
    (fulab.zarnidict.fulabdsl.core/main)
    fileset
    )
  )


(deftask dev
  "Development loop"
  []
  (comp
    ;(checkout :dependencies '[[regexpforobj "1.0.0-alpha2-SNAPSHOT"]])
    (watch)
    (speak)
    (reload)
    (wrap-reload "src")
    (main)
    )
  )
