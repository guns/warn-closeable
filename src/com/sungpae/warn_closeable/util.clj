(ns com.sungpae.warn-closeable.util
  "Functions for internal use."
  (:require [clojure.java.io :as io]
            [clojure.reflect :refer [type-reflect]]
            [clojure.string :as string]
            [clojure.tools.namespace.file :refer [read-file-ns-decl]]
            [clojure.tools.namespace.find :refer [find-namespaces]])
  (:import (clojure.lang LineNumberingPushbackReader)
           (java.io File PrintWriter StringWriter)
           (java.net URL URLClassLoader URLDecoder)))

(defn ns-symbol [ns]
  (case (class ns)
    #=clojure.lang.Namespace (ns-name ns)
    #=clojure.lang.Symbol ns))

(defn ^Class try-resolve [class-name]
  (cond (class? class-name) class-name
        (nil? class-name) nil
        :else (try
                (let [x (resolve class-name)]
                  (when (class? x)
                    x))
                (catch ClassNotFoundException _))))

(def ^:private ^Class BASE-INTERFACE
  "JRE 1.7+ introduced AutoCloseable for the try-with-resources feature."
  (or (try-resolve 'java.lang.AutoCloseable)
      java.io.Closeable))

(defn closeable-class?
  "Does this class implement (Auto)Closeable?"
  [cls]
  (and (class? cls)
       (.isAssignableFrom BASE-INTERFACE cls)))

(defn closeable-ctors
  "Use reflection to return a map of parameter counts to constructor param
   vectors that include at least one (Auto)Closeable instance. The contents
   of the param vectors should be considered boolean values: nil for
   non-closeable, Class for closeable."
  [^Class cls]
  (->> (type-reflect cls)
       :members
       (filter #(and (= (:name %) (symbol (.getCanonicalName cls)))
                     (contains? (:flags %) :public)))
       (map (fn [c]
              (mapv #(when-let [p (try-resolve %)]
                       (when (closeable-class? p)
                         p))
                    (:parameter-types c))))
       (filter (partial some identity))
       (reduce (fn [m params] (assoc m (count params) params)) {})))

(defn- classpath
  "System classpath as a sequence of string paths."
  []
  (for [^URL url (.getURLs ^URLClassLoader (ClassLoader/getSystemClassLoader))]
    (URLDecoder/decode (.getPath url) "UTF-8")))

(defn- clj? [^File f]
  (and (.isFile f)
       (.endsWith (.getPath f) ".clj")))

(defn project-namespace-symbols
  "Extract a sequence of ns symbols in *.clj files from a collection of
   paths, which may be a mix of files or directories, and may be any type
   implementing clojure.java.io/Coercions. If no paths are given, the entire
   classpath is searched."
  ([]
   (project-namespace-symbols (classpath)))
  ([paths]
   (mapcat
     (fn [path]
       (let [f (io/file path)]
         (cond (.isDirectory f) (find-namespaces [f])
               (clj? f) [(second (read-file-ns-decl f))])))
     paths)))

(defn ^LineNumberingPushbackReader namespace-reader
  "Return a reader on the resource corresponding to ns."
  [ns]
  (->> (str ns)
       (replace {\. \/ \- \_})
       string/join
       (format "%s.clj")
       io/resource
       io/reader
       LineNumberingPushbackReader.))

(defmacro with-reflection-warnings
  "Execute body and return tuple of:
   * Vector of reflection warning lines
   * Return value of body"
  [& body]
  `(let [sw# (new ~StringWriter)
         v# (binding [*warn-on-reflection* true
                      *out* (new ~PrintWriter sw#)]
              (do ~@body))
         ws# (->> (str sw#)
                  ~string/split-lines
                  (filterv #(.startsWith ^String % "Reflection warning")))]
     [ws# v#]))

(defn read-forms [rdr]
  (vec
    (take-while (partial not= ::done)
                (repeatedly #(read rdr false ::done)))))

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

(defn- print-error-line!
  "Pretty print an error-map.
   See closeable-warnings for the error-map schema."
  [error-map]
  (let [{:keys [line message form class]} error-map
        sb (cond->* (StringBuilder. "  ")
             line (.append (str line ": "))
             message (.append (str message " "))
             form (.append (str form " "))
             class (.append (str \[ class \])))]
    (println (str sb))))

(defn print-errors!
  "Pretty print a sequence of error maps.
   See closeable-warnings for the error-map schema."
  [error-maps]
  (doseq [[ns ws] (group-by :ns error-maps)]
    (let [{rs :reflection es nil} (group-by :type ws)]
      (when (seq es)
        (printf "[%s] ERRORS:\n" ns)
        (doseq [e es] (print-error-line! e)))
      (when (seq rs)
        (printf "[%s] REFLECTION WARNINGS:\n" ns)
        (doseq [r rs] (print-error-line! r))))))

(defn print-unclosed-warnings!
  "Pretty print a sequence of unclosed resource warning maps.
   See closeable-warnings for the warning map schema."
  [u-warnings]
  (doseq [[ns ws] (group-by :ns u-warnings)]
    (printf "[%s] Possibly unclosed (Auto)Closeable resources:\n" ns)
    (doseq [{:keys [line form class]} ws]
      (printf "  %d: %s [%s]\n" line form class))))
