(ns nrepl.redirect-stdout-stderr
  (:require [nrepl.middleware.session :as session]
            [nrepl.transport :as transport])
  (:import (java.io OutputStreamWriter)
           (java.util.function Consumer)
           (nrepl ConsumePrintStream)))

(defonce ^:private stdout-original System/out)
(defonce ^:private stderr-original System/err)

(defonce ^:private out-original *out*)
(defonce ^:private err-original *err*)

(defn- org-err-println [& args]
  (binding [*out* err-original]
    (apply println args)))

(defn- org-out-println [& args]
  (binding [*out* out-original]
    (apply println args)))

(def ^:const debug-stream-redirects?
  (if-let [value (System/getProperty "nrepl.redirect-stdout-stderr.debug")]
    (.equalsIgnoreCase value "true")
    false))

(defmacro ^:private when-debug?
  [expr]
  (if debug-stream-redirects?
    expr
    nil))

(defmacro ^:private debug-println
  [& args]
  (if debug-stream-redirects?
    `(org-err-println ~@args)
    nil))

(defn- redirect-stdout-stderr! [stdout-fn stderr-fn]
  (let [stdout-consumer (reify Consumer
                          (accept [_ line]
                            (org-out-println line)
                            (try
                              (stdout-fn line)
                              (catch Throwable t
                                (org-err-println "Error in stdout-fn:" (.getMessage t))
                                (.printStackTrace t stderr-original)))))
        stderr-consumer (reify Consumer
                          (accept [_ line]
                            (org-err-println line)
                            (try
                              (stderr-fn line)
                              (catch Throwable t
                                (org-err-println "Error in stderr-fn:" (.getMessage t))
                                (.printStackTrace t stderr-original)))))
        new-stdout (ConsumePrintStream. stdout-consumer)
        new-stderr (ConsumePrintStream. stderr-consumer)]
    (System/setOut new-stdout)
    (System/setErr new-stderr)
    (alter-var-root #'*out* (fn [_] (OutputStreamWriter. new-stdout)))
    (alter-var-root #'*err* (fn [_] (OutputStreamWriter. new-stderr)))))

(defonce ^:private sout-buf (atom []))
(defonce ^:private serr-buf (atom []))
(defonce ^:private lock (Object.))
(defonce ^:private bootstrapped? (atom false))

(defn- pr-str-inner [x]
  (binding [*print-dup* false
            *print-meta* false
            *print-readably* true
            *print-length* nil
            *print-level* nil
            *print-namespace-maps* false]
    (pr-str x)))

(defn- handle-line! [buf-dest kw-dest buffer? line]
  (locking lock
    (if-let [[transp sess-id] @session/most-recent-transport]
      (try
        (transport/send transp {:session sess-id
                                kw-dest (str line "\n")
                                :source :redirect})
        (catch Throwable t
          (debug-println "Transport send failed. Error message:" (.getMessage t))
          (reset! session/most-recent-transport nil)
          (when buffer? (swap! buf-dest conj line))))
      (if buffer?
        (do
          (debug-println "No session/transport yet, buffering line" (pr-str-inner line))
          (swap! buf-dest conj line))
        (debug-println "Buffering disabled. Dropping line" (pr-str-inner line))))))

(defn- drain-bufs-on-new-session [_ _ _os [transp sess-id :as ns]]
  (when (not (nil? ns))
    (future
      (Thread/sleep 1000)
      (when (= ns @session/most-recent-transport)
        (locking lock
          (doseq [line @sout-buf]
            (transport/send transp {:session sess-id
                                    :out (str line "\n")
                                    :source :redirect}))
          (doseq [line @serr-buf]
            (transport/send transp {:session sess-id
                                    :err (str line "\n")
                                    :source :redirect}))
          (debug-println "Pumped"
                         (+ (count @serr-buf) (count @sout-buf))
                         "lines"
                         "for session-id" sess-id)
          (reset! sout-buf [])
          (reset! serr-buf []))))))

(defn redirect! [buffer?]
  (locking lock
    (when (false? @bootstrapped?)
      (debug-println "Redirecting stdout and stderr. Buffer? is" buffer?)
      (redirect-stdout-stderr!
        (fn [line] (handle-line! sout-buf :out buffer? line))
        (fn [line] (handle-line! serr-buf :err buffer? line)))
      (when buffer?
        (add-watch session/most-recent-transport
                   ::watcher
                   drain-bufs-on-new-session))
      (reset! bootstrapped? true))))

(comment
  (future
    (loop []
      (.println System/out "janei..?")
      (Thread/sleep 1000)
      (recur))))
