(ns com.sungpae.warn-closeable
  "Contains a rudimentary linter for resource leaks.

   If an AutoCloseable object is created but not closed in a finally block
   immediately following the binding vector in which it is opened, a warning
   is issued.

   e.g.

     (ns example
       (:require [clojure.java.io :as io])
       (:import (java.security.cert CertificateFactory)))

     (defn make-certificates [x509-cert-file]
       (.generateCertificates (CertificateFactory/getInstance \"X.509\")
                              (io/input-stream x509-cert-file)))

   We run (warn-closeable! '[example]) to produce output like:

     {:ns example, :line 7, :form (io/input-stream x509-cert-file), :class java.io.InputStream}

   This serves as a reminder that the creation of an InputStream should be
   wrapped in with-open:

     (defn make-certificates [x509-cert-file]
       (with-open [input (io/input-stream x509-cert-file)]
         (.generateCertificates (CertificateFactory/getInstance \"X.509\") input)))
   "
  (:require [clojure.java.io :as io]
            [clojure.string :as string]
            [clojure.tools.analyzer :as ana]
            [clojure.tools.analyzer.ast :as ast]
            [clojure.tools.analyzer.jvm :as jvm]
            [clojure.tools.namespace.find :refer [find-namespaces]])
  (:import (clojure.lang ExceptionInfo LineNumberingPushbackReader Namespace)
           (java.io File)
           (java.lang AutoCloseable)
           (java.net URL URLClassLoader URLDecoder)))

(def ^:dynamic *nop-closeables*
  "Set of classes whose close methods are NOPs. StringReader#close is not a
   NOP, but since the resource is a String, it is often left unclosed."
  #{java.io.ByteArrayInputStream
    java.io.ByteArrayOutputStream
    java.io.StringReader
    java.io.StringWriter})

(defn- analyze [form]
  (binding [ana/macroexpand-1 jvm/macroexpand-1
            ana/create-var    jvm/create-var
            ana/parse         jvm/parse
            ana/var?          var?]
    (jvm/analyze form (jvm/empty-env))))

(defn- closeable?
  "Is this a fn or interop call that returns an AutoCloseable object?"
  [ast]
  (let [{:keys [op tag]} ast]
    (and (contains? #{:invoke :new :static-call :instance-call} op)
         (class? tag)
         (not (contains? *nop-closeables* tag))
         (.isAssignableFrom AutoCloseable tag))))

(defn- closing-call?
  "Is this a .close interop call on a local binding or a fn/method that
   returns an AutoCloseable object?"
  [ast]
  (and (= :instance-call (:op ast))
       (= 'close (:method ast))
       (let [inst (:instance ast)]
         (or (= :local (:op inst))
             (closeable? inst)))))

(defn- instance-form [ast]
  (-> ast :instance :form))

(defn- closed-in-scope?
  "Detects creation of a new Closeable resource in a let or loop, followed
   by a try/finally, with .close called in the finally clause. This is the
   macroexpansion of clojure.core/with-open, as well as good practice.

   e.g. (loop-or-let [rsrc (ctor)]
          (try
            body
            (finally
              (.close rsrc))))
   "
  [closeable-ast scope-ast]
  (let [{:keys [form]} closeable-ast
        [stmts ret] ((juxt :statements :ret) (-> scope-ast :body :ret :finally))]
    (->> (concat stmts [ret])
         (filter closing-call?)
         (map instance-form)
         (some #{form})
         boolean)))

(defn- unclosed-bindings
  "Return a vector of binding nodes in :bindings that do not have a paired
   closing call in the :body node."
  [ast]
  (filterv #(and (closeable? (:init %))
                 (not (closed-in-scope? % ast)))
           (:bindings ast)))

(defn- unclosed-resources
  "Return a vector of unclosed local bindings or Closeable fn/method calls at
   the top level of the node."
  [ast]
  (cond
    (contains? #{:let :loop} (:op ast)) (unclosed-bindings ast)
    (closeable? ast) [ast]
    :else []))

(defn- children
  "Returns a vector of the child nodes in ast, excluding those that are
   handled by #'unclosed-resources"
  [ast]
  (cond
    (contains? #{:let :loop} (:op ast)) [(:body ast)]
    (closing-call? ast) []
    :else (ast/children ast)))

(defn- find-unclosed-resources
  "Recurse through ast and return a vector of all unclosed nodes."
  [ast]
  (binding [*print-length* nil *print-level* 8]
    (reduce into
            (unclosed-resources ast)
            (mapv find-unclosed-resources (children ast)))))

(defn closeable-warnings
  "Returns a vector of warnings sorted by line number. Warnings are
   PersistentArrayMaps with :ns, :line, :form, and :class entries."
  [^Namespace ns]
  (binding [*ns* ns]
    (with-open [rdr (->> (str ns)
                         (replace {\. \/ \- \_})
                         string/join
                         (format "%s.clj")
                         io/resource
                         io/reader
                         LineNumberingPushbackReader.)]
      (let [form (take-while (partial not= ::done)
                             (repeatedly #(read rdr false ::done)))
            errors (find-unclosed-resources (analyze form))]
        (vec
          (for [ast errors
                :let [{:keys [form tag env]} ast
                      {:keys [ns line]} env
                      value (-> ast :init :form)]]
            (array-map :ns ns
                       :line line
                       :form (if value [form value] form)
                       :class tag)))))))

(defn- classpath []
  (for [^URL url (.getURLs ^URLClassLoader (ClassLoader/getSystemClassLoader))]
    (URLDecoder/decode (.getPath url) "UTF-8")))

(defn project-namespace-symbols
  "Extract a sequence of ns symbols in *.clj files from a collection of
   paths, which may be a mix of files or directories, and may be any type
   implementing clojure.java.io/Coercions. If no paths are given, the entire
   classpath is searched."
  ([]
   (project-namespace-symbols (classpath)))
  ([paths]
   (->> paths
        (map io/file)
        (filter (fn [^File f]
                  (or (.isDirectory f)
                      (and (.isFile f) (.endsWith (.getPath f) ".clj")))))
        find-namespaces)))

(defn warn-closeable!
  "Iterate through ns-syms and print the results of closeable-warnings on the
   namespace. If no namespace symbols are given, all project namespaces on the
   classpath are linted."
  ([]
   (apply warn-closeable! (sort (project-namespace-symbols))))
  ([& ns-syms]
   (doseq [ns-sym ns-syms]
     (try
       (require ns-sym)
       (let [ns (find-ns ns-sym)]
         (doseq [w (closeable-warnings ns)]
           (prn w)))
       (catch ExceptionInfo e
         (let [{:keys [column line class ast]} (.data e)]
           (prn (array-map :ns ns-sym
                           :error (.getMessage e)
                           :line line
                           :column column
                           :form (:form ast)
                           :class class))))
       (catch Throwable e
         (printf "[%s] %s\n" ns-sym e))))))
