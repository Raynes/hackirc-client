(ns hackirc.core
  (:require [clojure.java.io :as io]
            [clojure.string :as s])
  (:import java.net.Socket
           java.io.IOException))

(defn make-connection [host port]
  (let [socket (Socket. host port)]
    {:socket socket
     :reader (io/reader socket)
     :writer (io/writer socket)}))

;; Copied from irclj. Yes. I've basically written this library before.
;; It's not cheating, I swear.
(defn safe-line-seq
  "Get an infinite lazy sequence of lines from a reader."
  [rdr]
  (try
    (cons (.readLine rdr) (lazy-seq (safe-line-seq rdr)))
    (catch IOException _ nil)))

(defn read-lines [conn]
  (safe-line-seq (:reader conn)))

(defn write-line [conn line]
  (let [writer (:writer conn)]
    (doto writer
      (.write line)
      (.newLine)
      (.flush))))

(defmulti process :command)

(defmethod process "OK" [command]
  (println "OK"))

(defmethod process "ERROR" [command]
  (println (format "There was an error! %s" (s/join " " (:args command)))))

(defmethod process "WHISPER" [command]
  (let [[user & words] (:args command)]
    (println (format "Whisper from %s: %s" user (s/join " " words)))))

(defmethod process "MESSAGE" [command]
  (let [[channel user & words] (:args command)]
    (println (format "Message from %s in %s: %s"
                     user
                     channel
                     (s/join " " words)))))

(defn parse-line [line]
  (let [[command args] (s/split line #" " 2)
        args (rest (s/split line #" "))]
    {:command command
     :raw line
     :args args}))

(defn login [host port user]
  (let [conn (make-connection host port)]
    (.start (Thread. (fn []
                       (doseq [line (read-lines conn)]
                         (process (parse-line line))))))
    (write-line conn (format "LOGIN %s" user))
    conn))

(defn logout [conn]
  (write-line conn "LOGOUT")
  (.close (:socket conn)))

(defn whisper [conn user line]
  (write-line conn (format "WHISPER %s %s" user line)))

(defn say [conn channel line]
  (write-line conn (format "SAY %s %s" channel line)))

(defn join [conn channel]
  (write-line conn (format "JOIN %s" channel)))
