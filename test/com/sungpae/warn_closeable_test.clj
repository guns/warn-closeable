(ns com.sungpae.warn-closeable-test
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as string]
            [clojure.test :refer [deftest is testing]]
            [com.sungpae.warn-closeable :refer [closeable-warnings]]))

(defn has-warnings [clj-string warnings]
  (let [name (second (edn/read-string clj-string))
        path (->> (str name)
                  (replace {\- \_ \. \/})
                  string/join
                  (format "test/%s.clj"))
        ;; List equality fails with :static-call form for an unknown reason
        munge (fn [coll] (mapv #(update-in % [:form] str) coll))]
    (try
      (spit path clj-string)
      (require name :reload)
      (is (= (munge warnings)
             (munge (closeable-warnings (find-ns name)))))
      (finally
        (remove-ns name)
        (io/delete-file path :silently true)))))

(deftest test-closeable-invoke
  (has-warnings
    "(ns example)
     (defn foo [x] (clojure.java.io/input-stream x))"
    [{:ns 'example
      :line 2
      :form '(clojure.java.io/input-stream x)
      :class java.io.InputStream}]))

(deftest test-closeable-new
  (has-warnings
    "(ns example)
     (defn foo [x] (java.net.Socket.))"
    [{:ns 'example
      :line 2
      :form '(new java.net.Socket)
      :class java.net.Socket}]))

(deftest test-closeable-static-call
  (has-warnings
    "(ns example
       (:import (java.nio.file Files StandardOpenOption)))
     (defn foo [^java.io.File file]
       (Files/newByteChannel
         (.toPath file) (make-array StandardOpenOption 0)))"
    [{:ns 'example
      :line 4
      :form '(. java.nio.file.Files (newByteChannel (.toPath file) (make-array StandardOpenOption 0)))
      :class java.nio.channels.SeekableByteChannel}]))

(deftest test-closeable-instance-call
  (has-warnings
    "(ns example)
     (defn foo [f host port]
       (let [ss (java.net.ServerSocket. port 0xff host)
             s (.accept ss)]
         (f s)))"
    [{:ns 'example
      :line 3
      :form '[s (. ss accept)]
      :class java.net.Socket}
     {:ns 'example
      :line 3
      :form '[ss (new java.net.ServerSocket port 0xff host)]
      :class java.net.ServerSocket}]))

(deftest test-closeable-multiple-bindings
  (has-warnings
    "(ns example
       (:require [clojure.java.io :as io]))
     (defn foo [input output]
       (let [rd (io/reader input)
             v :do-something
             wr (io/writer output)]
         (io/copy rd wr)))"
    [{:ns 'example
      :line 4
      :form '[rd (io/reader input)]
      :class java.io.Reader}
     {:ns 'example
      :line 4
      :form '[wr (io/writer output)]
      :class java.io.Writer}]))

(deftest test-closeable-ok
  (has-warnings
    "(ns example
       (:require [clojure.java.io :as io]))
     (defn foo [f₁ f₂ f₃ f₄]
       (with-open [rd₁ (io/reader f₁)
                   rd₂ (io/reader f₂)]
         (let [rd₃ (io/reader f₃)
               rd₄ (io/reader f₄)]
           (try
             [rd₁ rd₂ rd₃ rd₄]
             (finally
               (.close rd₄)
               (.close rd₃))))))"
    []))

(deftest test-closeable-close-in-binding
  (has-warnings
    "(ns example
       (:require [clojure.java.io :as io]))
     (defn foo [input]
       (let [rd (io/reader input)
             v :do-something
             _ (.close rd)]
         v))"
    []))

; TODO: The go macro tears apart forms
; (deftest test-closeable-go-blocks
;   (has-warnings
;     "(ns example
;        (:require [clojure.core.async :as a]
;                  [clojure.java.io :as io]))
;      (defn foo [ch file]
;        (a/go
;          (with-open [wr (io/writer file)]
;            (io/copy (<! ch) wr))))"
;     []))
