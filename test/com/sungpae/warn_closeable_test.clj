(ns com.sungpae.warn-closeable-test
  (:require [clojure.java.io :as io]
            [clojure.string :as string]
            [clojure.test :refer [deftest is testing]]
            [com.sungpae.warn-closeable :refer [closeable-warnings]]))

(defn has-warnings
  ([clj-string warnings]
   (has-warnings clj-string warnings []))
  ([clj-string warnings errors]
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
         (io/delete-file path :silently true))))))

(deftest test-closeable-invoke
  (has-warnings
    "(ns example)
     (defn foo [x]
       (hash (clojure.java.io/input-stream x)))"
    [{:ns 'example
      :line 3
      :form '(clojure.java.io/input-stream x)
      :class java.io.InputStream}]))

(deftest test-closeable-new
  (has-warnings
    "(ns example)
     (defn foo [x]
       (hash (java.net.Socket.)))"
    [{:ns 'example
      :line 3
      :form '(new java.net.Socket)
      :class java.net.Socket}]))

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
      :form '[ss (new java.net.ServerSocket port 0xff host)]
      :class java.net.ServerSocket}
     {:ns 'example
      :line 3
      :form '[s (. ss accept)]
      :class java.net.Socket}]))

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
      :class java.io.Reader}]))

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

(deftest test-closeable-nops
  (has-warnings
    "(ns example)
     (defn foo [^String s]
       (let [bais (java.io.ByteArrayInputStream (.getBytes s))
             baos (java.io.ByteArrayOutputStream.)
             cr (java.io.CharArrayReader. (char-array s))
             cw (java.io.CharArrayWriter.)
             sr (java.io.StringReader. s)
             sw (java.io.StringWriter.)]
         [bais baos cr cw sr sw]))"
    []))

(deftest test-closeable-immediate-close
  (has-warnings
    "(ns example)
     (defn foo [& args]
       (let [cmd ^\"[Ljava.lang.String;\" (into-array String args)
             proc (.exec (Runtime/getRuntime) cmd)]
         (.close (.getInputStream proc))
         proc))"
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
    "(ns example)
     (defn foo [^String x]
       (java.io.FileInputStream. x))
     (defn ^java.io.Reader bar
       ([^String x] (bar x nil))
       ([^String x ^String y]
        (clojure.java.io/reader
          (clojure.java.io/file x y))))
     (defn baz [^String x]
       (.read (foo (bar x))))"
    [{:ns 'example
      :line 10
      :form '(bar x)
      :class java.io.Reader}]
    [{:ns 'example
      :type :reflection
      :line 2
      :message "fn-method `foo [x]` missing type hint ^java.io.FileInputStream"}
     {:ns 'example
      :type :reflection
      :line 10
      :message "reference to field or no args method call read cannot be resolved"}]))

(deftest test-closeable-global-resources
  (has-warnings
    "(ns example
       (:import (java.net URL URLClassLoader URLDecoder)))
     (defn- classpath []
       (for [^URL url (.getURLs ^URLClassLoader (ClassLoader/getSystemClassLoader))]
         (URLDecoder/decode (.getPath url) \"UTF-8\")))"
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
