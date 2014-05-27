(ns com.sungpae.warn-closeable-test
  (:require [clojure.java.io :as io]
            [clojure.string :as string]
            [clojure.test :refer [deftest is testing]]
            [com.sungpae.warn-closeable :refer [closeable-warnings]]))

(defn has-warnings
  [clj-string warnings errors]
  (let [name (second (read-string clj-string))
        path (->> (str name)
                  (replace {\- \_ \. \/})
                  string/join
                  (format "test/%s.clj"))
        ;; List equality fails with :static-call forms
        form-str (fn [coll] (mapv #(update-in % [:form] str) coll))]
    (try
      (spit path clj-string)
      (binding [*warn-on-reflection* false]
        (require name :reload))
      (let [ns (find-ns name)
            [ws es] (closeable-warnings ns)]
        (is (= (form-str ws) (form-str warnings)))
        (is (= es errors)))
      (finally
        (remove-ns name)
        (io/delete-file path :silently true)))))

(deftest test-closeable-invoke
  (has-warnings
    "(ns example)
     (defn foo [x]
       (hash (clojure.java.io/input-stream x)))"
    [{:ns 'example
      :line 3
      :form '(clojure.java.io/input-stream x)
      :class java.io.InputStream}]
    []))

(deftest test-closeable-new
  (has-warnings
    "(ns example)
     (defn foo [x]
       (hash (java.net.Socket.)))"
    [{:ns 'example
      :line 3
      :form '(new java.net.Socket)
      :class java.net.Socket}]
    []))

(deftest test-closeable-static-call
  (has-warnings
    "(ns example
       (:import (java.nio.file Files StandardOpenOption)))
     (defn foo [^java.io.File file]
       (.size
         (Files/newByteChannel
           (.toPath file) (make-array StandardOpenOption 0))))"
    [{:ns 'example
      :line 5
      :form '(. java.nio.file.Files (newByteChannel (.toPath file) (make-array StandardOpenOption 0)))
      :class java.nio.channels.SeekableByteChannel}]
    []))

(deftest test-closeable-instance-call
  (has-warnings
    "(ns example)
     (defn foo [f host port]
       (let [ss (java.net.ServerSocket. port 0xff host)
             s (.accept ss)]
         (f s)))"
    [{:ns 'example
      :line 3
      :form '[ss (new java.net.ServerSocket port 0xff host)]
      :class java.net.ServerSocket}
     {:ns 'example
      :line 3
      :form '[s (. ss accept)]
      :class java.net.Socket}]
    []))

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
      :class java.io.Writer}]
    []))

(deftest test-closeable-nested-bindings
  (has-warnings
    "(ns example
       (:require [clojure.java.io :as io]))
     (defn foo [input]
       (let [rd (let [rd (io/reader input)]
                  rd)]
         (.read rd)))"
    [{:ns 'example
      :line 4
      :form '[rd (io/reader input)]
      :class java.io.Reader}]
    []))

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
    []
    []))

(deftest test-closeable-whitelist
  (has-warnings
    "(ns example
       (:import (java.net URLClassLoader)))
     (defn ^java.io.BufferedReader foo [^String s]
       (java.io.BufferedReader. s))
     (defn bar [^String s]
       (let [_ (java.io.ByteArrayInputStream (.getBytes s))
             _ (java.io.ByteArrayOutputStream.)
             _ (java.io.CharArrayReader. (char-array s))
             _ (java.io.CharArrayWriter.)
             sr (java.io.StringReader. s)
             _ (java.io.StringWriter.)
             _ ^URLClassLoader (ClassLoader/getSystemClassLoader)
             _ (java.io.PrintWriter. System/out true)
             _ (java.io.BufferedReader. (java.io.StringReader. s))
             _ (java.io.BufferedReader. sr)
             _ (java.io.BufferedReader. (java.io.FileReader. s))]
         true))"
    [{:ns 'example
      :line 6
      :form '[_ (new java.io.BufferedReader (java.io.FileReader. s))]
      :class java.io.BufferedReader}]
    []))

(deftest test-closeable-immediate-close
  (has-warnings
    "(ns example)
     (defn foo [& args]
       (let [cmd ^\"[Ljava.lang.String;\" (into-array String args)
             proc (.exec (Runtime/getRuntime) cmd)]
         (.close (.getInputStream proc))
         proc))"
    []
    []))

(deftest test-closeable-pretty-reflection-errors
  (has-warnings
    "(ns example)
     (defn foo [x]
       (boolean (.getInputStream x)))"
    []
    [{:ns 'example
      :type :reflection
      :line 3
      :message "reference to field or no args method call getInputStream cannot be resolved"}]))

(deftest test-closeable-returns-resource
  (has-warnings
    "(ns example
       (:require [schema.core :as s]))
     (defn foo [^String x]
       (java.io.FileInputStream. x))
     (defn ^java.io.Reader bar
       ([^String x] (bar x nil))
       ([^String x ^String y]
        (clojure.java.io/reader
          (clojure.java.io/file x y))))
     (defn baz [^String x]
       (.read (foo (bar x))))
     (s/defn console-reader :- java.io.BufferedReader
       []
       (java.io.BufferedReader. (java.io.InputStreamReader. System/in \"UTF-8\")))"
    [{:ns 'example
      :line 11
      :form '(bar x)
      :class java.io.Reader}]
    [{:ns 'example
      :type :reflection
      :line 3
      :message "fn-method `foo [x]` missing type hint ^java.io.FileInputStream"}
     {:ns 'example
      :type :reflection
      :line 11
      :message "reference to field or no args method call read cannot be resolved"}]))

(deftest test-closeable-inner-form-tags
  (has-warnings
    "(ns example)
     (defn foo [^String x]
       (.toString (clojure.java.io/reader x))
       (.toString (java.io.FileInputStream. x))
       (.toString (.accept (java.net.ServerSocket. 80 0xff x)))
       (.toString (java.nio.file.Files/newByteChannel
                    (.toPath (java.io.File. x))
                    (make-array java.nio.file.StandardOpenOption 0))))"
    [{:ns 'example
      :line 3
      :form '(clojure.java.io/reader x)
      :class java.io.Reader}
     {:ns 'example
      :line 4
      :form '(new java.io.FileInputStream x)
      :class java.io.FileInputStream}
     {:ns 'example
      :line 5
      :form '(. (java.net.ServerSocket. 80 0xff x) accept)
      :class java.net.Socket}
     {:ns 'example
      :line 6
      :form '(. java.nio.file.Files
                (newByteChannel (.toPath (java.io.File. x))
                                (make-array java.nio.file.StandardOpenOption 0)))
      :class java.nio.channels.SeekableByteChannel}]
    []))

; TODO: If we do this, it must be more flexible
; (deftest test-closeable-close-in-binding
;   (has-warnings
;     "(ns example
;        (:require [clojure.java.io :as io]))
;      (defn foo [input]
;        (let [rd (io/reader input)
;              v :do-something
;              _ (.close rd)]
;          v))"
;     []))

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
