(ns sdk-generator.zip
  (:require [clojure.java.io :as io])
  (:import [java.util.zip ZipOutputStream ZipEntry]))

(defmacro with-entry [zip entry-name & body]
  `(let [^ZipOutputStream zip# ~zip]
     (.putNextEntry zip# (ZipEntry. ~entry-name))
     ~@body
     (flush)
     (.closeEntry zip#)))

(defmacro with-archive [[var file-name] & body]
  `(with-open [file# (io/output-stream ~file-name)
               ~var   (ZipOutputStream. file#)
               wrt#   (io/writer ~var)]
     (binding [*out* wrt#]
       ~@body)))
