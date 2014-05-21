```
                                    __                         __    __
.--.--.--.---.-.----.-----.___.----|  |-----.-----.-----.---.-|  |--|  |-----.
|  |  |  |  _  |   _|     |___|  __|  |  _  |__ --|  -__|  _  |  _  |  |  -__|
|________|___._|__| |__|__|   |____|__|_____|_____|_____|___._|_____|__|_____|
```

# WIP

A simple linter for Clojure namespaces that detects _possibly_ unclosed
instances of (Auto)Closeable objects.

This library will optimally be included as an Eastwood linter.

## Usage

Available as a Leiningen plugin or as a library for use from a REPL.

* Leiningen plugin:

```clojure
;; ~/.lein/profiles.clj
;; {:user {:plugins [[lein-warn-closeable "0.1.0"]]}}
"NOT AVAILABLE; INSTALL FROM SOURCE"
```

  Run `lein warn-closeable` from your project root.

* REPL usage

```clojure
;; ~/.lein/profiles.clj
;; {:user {:dependencies [[com.sungpae/warn-closeable "0.1.0"]]}}
"NOT AVAILABLE; INSTALL FROM SOURCE"
```

  Then:

```clojure
(require '[com.sungpae.warn-closeable :as wc])

(wc/closeable-warnings *ns*) ; Lint current namespace
(wc/warn-closeable!)         ; Lint all namespaces, same as `lein warn-closeable`
```

## Rationale

The [Closeable][] interface (and the [AutoCloseable][] interface in JRE 1.7+)
represents a resource that must be manually closed, like open files and
network sockets.

While the `.close` method of (Auto)Closeable objects are often called in the
object finalizer, there is no guarantee that objects will ever be finalized.
Therefore, resources must be explicitly closed to avoid resource leaks, which
in turn may lead to undesirable exceptions like:

```
java.io.FileNotFoundException: (Too many open files)
```

A common convention in JVM languages is to close an (Auto)Closeable object in
a `finally` block after opening the resource:

```clojure
(let [s (java.net.Socket.)]
  (try
    …
    (finally
      (.close s))))
```

This is usually written in Clojure with the `with-open` macro, which expands
to the same code as above:

```clojure
(with-open [s (java.net.Socket.)]
  …)
```

`warn-closeable` and `lein-warn-closeable`, leveraging the power of
[tools.analyzer][], can detect when an (Auto)Closeable object has been created
without an associated `.close` call in a finally block.

MORE DOCUMENTATION HERE

[Closeable]: http://docs.oracle.com/javase/8/docs/api/java/io/Closeable.html
[AutoCloseable]: http://docs.oracle.com/javase/8/docs/api/java/lang/AutoCloseable.html
[tools.analyzer]: https://github.com/clojure/tools.analyzer
