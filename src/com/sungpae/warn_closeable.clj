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

(defn ^:private analyze [form]
  (binding [ana/macroexpand-1 jvm/macroexpand-1
            ana/create-var    jvm/create-var
            ana/parse         jvm/parse
            ana/var?          var?]
    (jvm/analyze form (jvm/empty-env))))

(defn ^:private closeable-opening-form? [ast]
  (let [{:keys [op tag]} ast]
    (and (contains? #{:invoke :new :static-call :instance-call} op)
         (class? tag)
         (not (contains? *nop-closeables* tag))
         (.isAssignableFrom AutoCloseable tag))))

(defn ^:private close-call? [ast]
  (and (= :instance-call (:op ast))
       (= 'close (:method ast))))

(defn ^:private instance-sym [ast]
  (-> ast :instance :form))

(defn ^:private closed-in-scope?
  "Detects resource management in a let or loop, followed by a try/finally,
   with .close called in the finally clause. This is the macroexpansion of
   clojure.core/with-open, as well as good practice.

   e.g. (loop-or-let [rsrc (ctor)]
          (try
            body
            (finally
              (.close rsrc))))

   If a resource is closed in the same binding vector in which it is opened
   (this happens in the macroexpansion of a core.async go block), this is
   detected as well.

   e.g. (let [rsrc (ctor)
              x (f rsrc)
              _ (.close rsrc)]
          …)
   "
  [resource-ast scope-ast next-nodes]
  (let [{:keys [form]} resource-ast
        [stmts ret] ((juxt :statements :ret) (-> scope-ast :body :ret :finally))
        inits (map :init next-nodes)]
    (->> (concat inits stmts [ret])
         (filter close-call?)
         (map instance-sym)
         (some #{form})
         boolean)))

(defn ^:private add-unclosed-nodes [unclosed node next-nodes]
  (->> (:bindings node)
       (filter #(and (closeable-opening-form? (:init %))
                     (not (closed-in-scope? % node next-nodes))))
       (into unclosed)))

(defn ^:private find-unclosed-resources [form]
  (loop [unclosed #{} [node & more] (ast/nodes (analyze form))]
    (if node
      (if (closeable-opening-form? node)
        ;; A Closeable form outside of a binding vector is considered unclosed
        (recur (conj unclosed node) more)
        (if (contains? #{:let :loop} (:op node))
          ;; Process this binding form, then skip ahead to the body
          (let [[bindings more] (split-with #(not= :do (:op %)) more)]
            (recur (add-unclosed-nodes unclosed node bindings) more))
          (recur unclosed more)))
      unclosed)))

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
            errors (find-unclosed-resources form)
            ws (for [ast errors
                     :let [{:keys [form tag env]} ast
                           {:keys [ns line]} env
                           value (-> ast :init :form)]]
                 (array-map :ns ns
                            :line line
                            :form (if value [form value] form)
                            :class tag))]
        (vec (sort-by (juxt :line :form) ws))))))

(defn ^:private classpath []
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
   (warn-closeable! (sort (project-namespace-symbols))))
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
