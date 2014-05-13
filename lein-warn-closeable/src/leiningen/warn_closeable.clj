(ns leiningen.warn-closeable
  "Warn on potentially unclosed AutoCloseable resources"
  (:require [leiningen.core.eval :refer [eval-in-project]]
            [leiningen.core.project :refer [merge-profiles]]))

(def warn-closeable-profile
  '{:dependencies [[com.sungpae/warn-closeable "0.1.0-SNAPSHOT"]]})

(defn warn-closeable
  "Arguments are files and directories containing Clojure source files. If no
   arguments are given, all project namespaces are linted."
  [project & src-paths]
  (let [profile (or (:warn-closeable project) warn-closeable-profile)
        project (merge-profiles project [profile])]
    (eval-in-project project
      `(com.sungpae.warn-closeable/warn-closeable! ~@src-paths)
      `(require 'com.sungpae.warn-closeable))))
