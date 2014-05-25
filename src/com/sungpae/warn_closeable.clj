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
  (:require [clojure.tools.analyzer.ast :as ast]
            [clojure.tools.analyzer.jvm :as jvm]
            [com.sungpae.warn-closeable.util :refer [closeable-class?
                                                     closeable-ctors
                                                     namespace-reader prewalk
                                                     print-errors!
                                                     print-unclosed-warnings!
                                                     project-namespace-symbols
                                                     read-forms try-resolve
                                                     with-reflection-warnings]])
  (:import (clojure.lang ExceptionInfo Namespace)))

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
  "Map of (Auto)Closeable classes that wrap other resources.

     Returns: {Class {Int [closeable-ctor-params]}}

   cf. Eclipse: TypeConstants.JAVA_IO_WRAPPER_CLOSEABLES, etc.
   Copyright (c) 2012, 2013 Eclipse Foundation and others."
  (reduce
    (fn [m k]
      (if-let [c (try-resolve k)]
        (assoc m c (closeable-ctors c))
        m))
    {} ["clojure.lang.LineNumberingPushbackReader"
        "java.io.BufferedInputStream"
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
    (for [form '[(. java.lang.System -in)
                 (. java.lang.System in)
                 (. java.lang.System -out)
                 (. java.lang.System out)
                 (. java.lang.System -err)
                 (. java.lang.System err)
                 (. java.lang.ClassLoader getSystemClassLoader)]]
      (str form))))

(defn- call-tag [ast]
  ;; TODO: Investigate :tag vs :o-tag vs :class
  (let [{:keys [class o-tag tag op]} ast]
    (case op
      :invoke (or tag o-tag class)
      (or class o-tag tag))))

(declare whitelisted-closeable?)

(defn- whitelisted-ctor-args?
  "Validate the matching ctor of cls with args."
  [cls args]
  (when-let [ctor (get-in *closeable-wrappers* [cls (count args)])]
    (loop [v true params ctor [arg & more] args]
      ;; The ctor vector includes nil values
      (if (and v (seq params))
        (if (first params)
          ;; This is an (Auto)Closeable parameter, so allow resource free
          ;; (Auto)Closeable objects and global resources
          (recur (whitelisted-closeable? arg) (rest params) more)
          ;; Other parameters must not be any kind of (Auto)Closeable object
          (recur (not (closeable-class? (call-tag arg))) (rest params) more))
        v))))

(defn- whitelisted-closeable?
  "Is this node either:

   - A resource-free (Auto)Closeable object?
   - A known form that returns a global resource?
   - An (Auto)Closeable ctor invocation that wraps nodes that are also
     whitelisted-closeable?

   If the second optional boolean param is false, the first check is skipped."
  [ast]
  (let [{:keys [op form]} ast
        cls (call-tag ast)]
    (if (contains? *closeable-wrappers* cls)
      (case op
        :new (whitelisted-ctor-args? cls (:args ast))
        false)
      (or
        (contains? *resource-free-closeables* cls)
        (contains? *system-resource-forms* (str form))))))

(defn- closeable?
  "Is this an (Auto)Closeable object?"
  [ast]
  (and (closeable-class? (:tag ast))
       (not (whitelisted-closeable? ast))))

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

(defn- lint-bindings
  "unclosed-resources helper for :let and :loop nodes."
  [ast]
  [(unclosed-bindings ast)
   []
   (conj (non-closeable-bindings ast) (:body ast))])

(defn- lint-defn
  "unclosed-resources helper for :def nodes that wrap a :fn node. Only checks
   for missing (Auto)Closeable type hints on the :def node."
  [ast]
  (let [defname (:name ast)
        ^Class deftag (or (-> ast :meta :val :tag)
                          (when-let [sym (-> ast :meta :form :tag)]
                            (try-resolve (str sym))))
        fn-methods (-> ast :init :methods)]
    (reduce
      (fn [[_ errors children] fn-method]
        (if (closeable? (:body fn-method))
          (if (= deftag (:tag (:body fn-method)))
            [_ errors children]
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
              [[] (conj errors e) children]))
          [[] errors (conj children fn-method)]))
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

(defn- preserve-type-hints
  "Return a new form with :tag metadata entries duplicated to
   :com.sungpae.warn-closeable/tag"
  [form]
  (prewalk #(if-let [m (meta %)]
              (with-meta % (assoc m ::tag (:tag m)))
              %)
           form))

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

   The namespace is required before linting. Any arguments after the namespace
   are passed to require as flags (e.g. :reload, :reload-all, and :verbose)."
  [^Namespace namespace & require-flags]
  (binding [*ns* namespace]
    (let [ns-sym (ns-name namespace)]
      (try
        (binding [*warn-on-reflection* false]
          (apply require ns-sym require-flags))
        (let [forms (with-open [rdr (namespace-reader namespace)]
                      (read-forms rdr))
              [rs [nodes errors]] (with-reflection-warnings
                                    (-> forms
                                        preserve-type-hints
                                        jvm/analyze
                                        find-unclosed-resources))
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
                :message (str e)}]])))))

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
       (let [[warnings errors] (closeable-warnings (find-ns ns-sym))]
         (print-errors! errors)
         (print-unclosed-warnings! warnings))
       (catch Throwable e
         (printf "ERROR: %s\n" e))))))
