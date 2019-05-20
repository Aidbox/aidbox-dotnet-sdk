(ns sdk-generator.utils)

(defn uuid []
  (.toString (java.util.UUID/randomUUID)))

(defmacro resource [name]
  `(slurp ~(str "resources/" name)))
