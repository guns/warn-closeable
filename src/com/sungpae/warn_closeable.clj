(ns com.sungpae.warn-closeable
  "Contains a rudimentary linter for resource leaks.

   If an (Auto)Closeable object is created but not closed in a finally block
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
            [clojure.reflect :refer [type-reflect]]
            [clojure.string :as string]
            [clojure.tools.analyzer :as ana]
            [clojure.tools.analyzer.ast :as ast]
            [clojure.tools.analyzer.jvm :as jvm]
            [clojure.tools.namespace.find :refer [find-namespaces]])
  (:import (clojure.lang ExceptionInfo LineNumberingPushbackReader Namespace)
           (java.io File PrintWriter StringWriter)
           (java.net URL URLClassLoader URLDecoder)))

(defn- ^Class try-resolve [class-name]
  (try
    (Class/forName class-name)
    (catch ClassNotFoundException _)))

(def ^:private ^Class BASE-INTERFACE
  "JRE 1.7+ introduced AutoCloseable for the try-with-resources feature."
  (or (try-resolve "java.lang.AutoCloseable")
      java.io.Closeable))

(def ^:dynamic *resource-free-closeables*
  "Set of (Auto)Closeable classes that do not allocate OS resources.

   cf. Eclipse: TypeConstants.JAVA_IO_RESOURCE_FREE_CLOSEABLES
   Copyright (c) 2012, 2013 Eclipse Foundation and others."
  (reduce
    (fn [s k] (if-let [c (try-resolve k)] (conj s c) s))
    #{} ["java.io.ByteArrayInputStream"
         "java.io.ByteArrayOutputStream"
         "java.io.CharArrayReader"
         "java.io.CharArrayWriter"
         "java.io.StringReader"
         "java.io.StringWriter"
         "java.io.StringBufferInputStream"
         "java.util.stream.Stream"]))

(def ^:dynamic *closeable-wrappers*
  "(Auto)Closeable classes that wrap other resources, mapped to a sequence of
   their constructor parameter types.

   cf. Eclipse: TypeConstants.JAVA_IO_WRAPPER_CLOSEABLES, etc.
   Copyright (c) 2012, 2013 Eclipse Foundation and others."
  (reduce
    (fn [m k]
      (if-let [c (try-resolve k)]
        (assoc m c (->> (type-reflect c)
                        :members
                        (filter #(= (:name %) (symbol (.getCanonicalName c))))
                        (map :parameter-types)))
        m))
    {} ["java.io.BufferedInputStream"
        "java.io.BufferedOutputStream"
        "java.io.BufferedReader"
        "java.io.BufferedWriter"
        "java.io.InputStreamReader"
        "java.io.PrintWriter"
        "java.io.LineNumberReader"
        "java.io.DataInputStream"
        "java.io.DataOutputStream"
        "java.io.ObjectInputStream"
        "java.io.ObjectOutputStream"
        "java.io.FilterInputStream"
        "java.io.FilterOutputStream"
        "java.io.PushbackInputStream"
        "java.io.SequenceInputStream"
        "java.io.PrintStream"
        "java.io.PushbackReader"
        "java.io.OutputStreamWriter"
        "java.util.zip.GZIPInputStream"
        "java.util.zip.InflaterInputStream"
        "java.util.zip.DeflaterInputStream"
        "java.util.zip.CheckedInputStream"
        "java.util.zip.ZipInputStream"
        "java.util.jar.JarInputStream"
        "java.util.zip.GZIPOutputStream"
        "java.util.zip.InflaterOutputStream"
        "java.util.zip.DeflaterOutputStream"
        "java.util.zip.CheckedOutputStream"
        "java.util.zip.ZipOutputStream"
        "java.util.jar.JarOutputStream"
        "java.security.DigestInputStream"
        "java.security.DigestOutputStream"
        "java.beans.XMLEncoder"
        "java.beans.XMLDecoder"
        "javax.sound.sampled.AudioInputStream"]))

(def ^:dynamic *system-resource-forms*
  "Set of forms that are known to return global (Auto)Closeable resources that
   should not be closed."
  (set
    (for [form `[(ClassLoader/getSystemClassLoader)]]
      (str (macroexpand form)))))

(defn- analyze [form]
  (binding [ana/macroexpand-1 jvm/macroexpand-1
            ana/create-var    jvm/create-var
            ana/parse         jvm/parse
            ana/var?          var?]
    (jvm/analyze form (jvm/empty-env))))

;; Copied from Clojure 1.6.0, Copyright (c) Rich Hickey
(defmacro ^:private cond->*
  "Takes an expression and a set of test/form pairs. Threads expr (via ->)
   through each form for which the corresponding test
   expression is true. Note that, unlike cond branching, cond-> threading does
   not short circuit after the first true test expression."
  {:added "1.5"}
  [expr & clauses]
  (assert (even? (count clauses)))
  (let [g (gensym)
        pstep (fn [[test step]] `(if ~test (-> ~g ~step) ~g))]
    `(let [~g ~expr
           ~@(interleave (repeat g) (map pstep (partition 2 clauses)))]
       ~g)))

(defn- whitelisted-closeable? [ast]
  (let [{:keys [class o-tag tag op form]} ast
        ;; TODO: Investigate :tag vs :o-tag vs :class
        cls (case op
              :invoke (or tag o-tag class)
              (or class o-tag tag))]
    (if (contains? *closeable-wrappers* cls)
      (case op
        (:new :invoke) (every? whitelisted-closeable? (:args ast))
        false)
      (or
        (contains? *resource-free-closeables* cls)
        (contains? *system-resource-forms* (str form))))))

(defn- closeable?
  "Is this an (Auto)Closeable object?"
  [ast]
  (let [{:keys [tag]} ast]
    (and (class? tag)
         (.isAssignableFrom BASE-INTERFACE tag)
         (not (whitelisted-closeable? ast)))))

(defn- closeable-call?
  "Is this a fn or interop call that returns an (Auto)Closeable object?"
  [ast]
  (and (contains? #{:invoke :new :static-call :instance-call} (:op ast))
       (closeable? ast)))

(defn- closing-call?
  "Is this a .close interop call on a local binding or a fn/method that
   returns an (Auto)Closeable object?"
  [ast]
  (and (= :instance-call (:op ast))
       (= 'close (:method ast))
       (let [inst (:instance ast)]
         (or (= :local (:op inst))
             (closeable-call? inst)))))

(defn- instance-form [ast]
  (-> ast :instance :form))

(defn- closed-in-scope?
  "Detects creation of a new (Auto)Closeable resource in a let or loop,
   followed by a try/finally, with .close called in the finally clause. This
   is the macroexpansion of clojure.core/with-open, as well as good practice.

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
  (filterv #(and (closeable-call? (:init %))
                 (not (closed-in-scope? % ast)))
           (:bindings ast)))

(defn- non-closeable-bindings
  "Return a vector on binding nodes that do not bind (Auto)Closeable calls."
  [ast]
  (filterv (comp not closeable-call? :init) (:bindings ast)))

(defn- lint-bindings [ast]
  [(unclosed-bindings ast)
   []
   (conj (non-closeable-bindings ast) (:body ast))])

(defn- lint-defn [ast]
  (let [defname (:name ast)
        ^Class deftag (or (-> ast :meta :val :tag)
                          (when-let [sym (-> ast :meta :form :tag)]
                            (resolve sym)))
        fn-methods (-> ast :init :methods)]
    (reduce
      (fn [[unclosed errors children] fn-method]
        (if (closeable? (:body fn-method))
          (if (= deftag (:tag (:body fn-method)))
            [unclosed errors children]
            (let [args (:arglist fn-method)
                  e {:ns (ns-name *ns*)
                     :type :reflection
                     :line (-> fn-method :env :line)
                     :message (let [ret (.getCanonicalName ^Class (:tag (:body fn-method)))]
                                (if (nil? deftag)
                                  (format "fn-method `%s %s` missing type hint ^%s"
                                          defname args ret)
                                  (format "fn-method `%s %s` tagged as %s, but returns %s"
                                          defname args (.getCanonicalName deftag) ret)))}]
              [unclosed (conj errors e) children]))
          [unclosed errors (conj children fn-method)]))
      [[] [] [(:meta ast)]] fn-methods)))

(defn- unclosed-resources
  "Return a tuple of:

   * Top level unclosed local bindings or (Auto)Closeable fn/method calls
   * Error maps
   * Child nodes that should be investigated."
  [ast]
  (cond
    ;; Check local bindings and do not recurse into unclosed values
    (contains? #{:let :loop} (:op ast)) (lint-bindings ast)
    ;; Ensure function vars that return resources are tagged appropriately
    (and (= :def (:op ast))
         (= :fn (:op (:init ast)))) (lint-defn ast)
    ;; Ignore function bodies that return resources
    (= :fn-method (:op ast)) (if (closeable? (:body ast))
                               [[] [] []]
                               [[] [] (ast/children ast)])
    ;; Mark resources created outside of a local binding as unclosed
    (closeable-call? ast) [[ast] [] []]
    ;; Do not recurse into the inner form of a .close call
    (closing-call? ast) [[] [] []]
    ;; Otherwise recurse into all child nodes
    :else [[] [] (ast/children ast)]))

(defn- find-unclosed-resources
  "Traverse ast and return a vector of all unclosed nodes."
  [ast]
  (let [[unclosed errors children] (unclosed-resources ast)]
    (reduce
      (fn [vs vs']
        (-> vs
            (update-in [0] into (vs' 0))
            (update-in [1] into (vs' 1))))
      [unclosed errors] (mapv find-unclosed-resources children))))

(defn- ^LineNumberingPushbackReader namespace-reader [ns]
  (->> (str ns)
       (replace {\. \/ \- \_})
       string/join
       (format "%s.clj")
       io/resource
       io/reader
       LineNumberingPushbackReader.))

(defmacro ^:private with-reflection-warnings [& body]
  `(let [sw# (new ~StringWriter)
         v# (binding [*warn-on-reflection* true
                      *out* (new ~PrintWriter sw#)]
              (do ~@body))
         ws# (->> (str sw#)
                  ~string/split-lines
                  (filterv #(.startsWith ^String % "Reflection warning")))]
     [ws# v#]))

(defn- read-forms [rdr]
  (vec
    (take-while (partial not= ::done)
                (repeatedly #(read rdr false ::done)))))

(defn closeable-warnings
  "Detect potentially unclosed (Auto)Closeable objects.

   Returns a tuple of:

   * Unclosed resource warnings:

     [{:ns Symbol
       :line Int
       :form Sexp
       :class Class}]

   * Reflection warnings and other errors:

     [{:ns Symbol
       :type Keyword
       :line (maybe Int)
       :form (maybe Sexp)
       :class (maybe Class)
       :message String}]
   "
  [^Namespace namespace]
  (binding [*ns* namespace]
    (with-open [rdr (namespace-reader namespace)]
      (let [ns-sym (ns-name namespace)]
        (try
          (let [[rs [nodes errors]] (with-reflection-warnings
                                      (find-unclosed-resources
                                        (analyze (read-forms rdr))))
                ws (for [ast nodes
                         :let [{:keys [form tag env]} ast
                               {:keys [ns line]} env
                               value (-> ast :init :form)]]
                     {:ns ns
                      :line line
                      :form (if value [form value] form)
                      :class tag})
                es (for [r rs
                         :let [[_ l m] (re-find #"\S+:(\d+):[\d\s]*- (.*)" r)]]
                     {:ns ns-sym
                      :type :reflection
                      :line (Long/parseLong l)
                      :message m})]
            [(vec ws) (into errors es)])
          (catch ExceptionInfo e
            (let [{:keys [line class ast]} (.data e)]
              [[] [{:ns ns-sym
                    :line line
                    :form (:form ast)
                    :class class
                    :message (.getMessage e)}]]))
          (catch Throwable e
            [[] [{:ns ns-sym
                  :message (str e)}]]))))))

(defn- classpath []
  (for [^URL url (.getURLs ^URLClassLoader (ClassLoader/getSystemClassLoader))]
    (URLDecoder/decode (.getPath url) "UTF-8")))

(defn ^:private project-namespace-symbols
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

(defn- print-error-line! [error]
  (let [{:keys [line message form class]} error
        sb (cond->* (StringBuilder. "  ")
             line (.append (str line ": "))
             message (.append (str message " "))
             form (.append (str form " "))
             class (.append (str \[ class \])))]
    (println (str sb))))

(defn- print-errors! [errors]
  (doseq [[ns ws] (group-by :ns errors)]
    (let [{rs :reflection es nil} (group-by :type ws)]
      (when (seq es)
        (printf "[%s] ERRORS:\n" ns)
        (doseq [e es] (print-error-line! e)))
      (when (seq rs)
        (printf "[%s] REFLECTION WARNINGS:\n" ns)
        (doseq [r rs] (print-error-line! r))))))

(defn- print-unclosed-warnings! [u-warnings]
  (doseq [[ns ws] (group-by :ns u-warnings)]
    (printf "[%s] Possibly unclosed (Auto)Closeable resources:\n" ns)
    (doseq [{:keys [line form class]} ws]
      (printf "  %d: %s [%s]\n" line form class))))

(defn warn-closeable!
  "Iterate through ns-syms and print the results of closeable-warnings on the
   namespace. If no namespace symbols are given, all project namespaces on the
   classpath are linted.

   Reflection warnings in the namespace are also printed, as statically
   detecting (Auto)Closeable instances relies on properly type hinted code."
  ([]
   (apply warn-closeable! (project-namespace-symbols)))
  ([& ns-syms]
   (doseq [ns-sym ns-syms]
     (try
       (binding [*warn-on-reflection* false]
         (require ns-sym))
       (let [[warnings errors] (closeable-warnings (find-ns ns-sym))]
         (print-errors! errors)
         (print-unclosed-warnings! warnings))
       (catch Throwable e
         (printf "ERROR: %s\n" e))))))
