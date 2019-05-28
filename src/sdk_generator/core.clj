(ns sdk-generator.core
  (:require [sdk-generator.zip :as zip]
            [sdk-generator.utils :as u]
            [clojure.pprint :as pprint]
            [org.httpkit.client :as http]
            [cheshire.core :as json]
            [clojure.string :as str]))

(def project         (u/resource "project.tpl"))
(def assembly-info   (u/resource "assembly-info.tpl"))
(def resource-class  (u/resource "resource-class.tpl"))
(def property        (u/resource "property.tpl"))
(def nested-class    (u/resource "nested-class.tpl"))
(def value-types     {:integer     "System.Int64"
                      :unsignedInt "System.UInt64"
                      :unsignedint "System.UInt64"
                      :date        "System.DateTime"
                      :dateTime    "System.DateTime"
                      :instant     "System.DateTime"
                      :time        "System.DateTime"
                      :decimal     "System.Decimal"
                      :boolean     "System.Boolean"})
(def reference-types {:string "System.String"})
(def primitive-types (merge value-types reference-types))

(defn fmt [s & args]
  (apply pprint/cl-format true s args))

(defn sfmt [s & args]
  (apply pprint/cl-format nil s args))

(defn fmt-property [comment type name & [tabs]]
  (let [indent (* 4 (or tabs 2))
        s      (sfmt property comment type name)
        lines  (str/split-lines s)]
    (->> lines
         (map #(str (apply str (repeat indent " ")) %))
         (str/join \newline))))

(defn generate-project [uuid files]
  (fmt project uuid files))

(defn generate-assembly-info [uuid]
  (fmt assembly-info uuid))

;; https://forsdk.aidbox.app/$metadata
(defn find-resource-attributes [resource metadata]
  (->> metadata :Attribute
       (filter #(= (-> % second :resource :id)
                   (name resource)))
       (map second)))

(defn simple-field? [attribute]
  (and (= (count (:path attribute)) 1)
       (not= (:path attribute) ["adjudication"])    ; only for testing purposes
       (not (nil? (:type attribute)))
       (nil? (:union attribute))))

(defn primitive-field? [attribute]
  (and (simple-field? attribute)
       (-> attribute :type :id keyword primitive-types)))

(defn compound-field? [attribute]
  (or (> (count (:path attribute)) 1)
      (when-let [t (-> attribute :type :id)]
        (str/includes? t "."))))

(defn nullable-primitive-field? [attribute]
  (and (primitive-field? attribute)
       (-> attribute :type :id keyword value-types)
       (nil? (:isRequired attribute))))

(defn pascal-case [s]
  (str (str/upper-case (subs s 0 1)) (subs s 1)))

(defn field-type [attribute]
  (cond
    (:polymorphic? attribute)
    (:type attribute)

    (compound-field? attribute)
    nil

    (nullable-primitive-field? attribute)
    (str "System.Nullable<" (-> attribute :type :id keyword primitive-types) ">")

    (primitive-field? attribute)
    (-> attribute :type :id keyword primitive-types)
    
    (simple-field? attribute)
    (-> attribute :type :id pascal-case)

    ))

(defn find-polymorphic-attributes [attributes]
  (let [polymorphics (->> attributes
                          (filter :union)
                          (reduce (fn [acc attr]
                                    (assoc acc (-> attr :path first) attr))
                                  {}))
        polymorphics (->> attributes
                          (filter #(polymorphics (-> % :path first)))
                          (group-by (comp first :path))
                          (map (fn [[k attrs]]
                                 [{:path [k]
                                   :polymorphic? true
                                   :type (str (pascal-case k) "X")
                                   :description (-> k polymorphics :description)}
                                  attrs])))]
    polymorphics))

(defn group-attributes [attributes]
  (let [polymorphics (map first (find-polymorphic-attributes attributes))
        pset         (set (map (comp first :path) polymorphics))
        attributes   (remove #(pset (-> % :path first)) attributes)]
    (concat attributes polymorphics)))

(def reserved-words #{"abstract" "as" "base" "bool" "break" "byte" "case" "catch" "char" "checked" "class" "const" "continue" "decimal" "default" "delegate" "do" "double" "else" "enum" "event" "explicit" "extern" "false" "finally" "fixed" "float" "for" "foreach" "goto" "if" "implicit" "in" "int" "interface" "internal" "is" "lock" "long" "namespace" "new" "null" "object" "operator" "out" "override" "params" "private" "protected" "public" "readonly" "ref" "return" "sbyte" "sealed" "short" "sizeof" "stackalloc" "static" "string" "struct" "switch" "this" "throw" "true" "try" "typeof" "uint" "ulong" "unchecked" "unsafe" "ushort" "using" "virtual" "void" "volatile" "while" "add" "alias" "ascending" "async" "await" "by" "descending" "dynamic" "equals" "from" "get" "global" "group" "into" "join" "let" "nameof" "on" "orderby" "partial" "remove" "select" "set" "value" "var" "when" "where" "yield"})

(defn field-name [attribute]
  (let [name (-> attribute :path first)]
    (str (when (reserved-words name) "@") name)))

(defn generate-field [attribute & [tabs]]
  (let [comment (or (:description attribute) "")
        type    (field-type attribute)
        name    (field-name attribute)]
    (when type
      (fmt-property comment type name tabs))))

(defn entity-name [id]
  (-> id name pascal-case))

(defn generate-nested-class [[field attributes]]
  (let [name       (:type field)
        attributes (remove :union attributes)
        fields     (->> attributes
                        (map #(assoc % :path (rest (:path %))))
                        (map #(generate-field % 4))
                        (filter some?))]
    (sfmt nested-class name fields)))

(defn generate-nested-classes [attributes]
  (let [polymorphics (find-polymorphic-attributes attributes)]
    (map generate-nested-class polymorphics)))

(defn generate-resource-class [[id props] metadata]
  (let [comment (or (:description props) "")
        modifiers (if (= (:type props) "abstract")
                    "abstract"
                    "partial")
        class-name (entity-name id)
        base-class (case id
                     :Resource                                     ""
                     (:DomainResource :Bundle :Parameters :Binary) " : Resource"
                     " : DomainResource")
        attributes (-> id
                       (find-resource-attributes metadata)
                       (group-attributes))
        class-fields (->> attributes
                          (map generate-field)
                          (filter some?))
        nested-classes (-> id
                           (find-resource-attributes metadata)
                           (generate-nested-classes))]
    (fmt resource-class comment modifiers class-name base-class nested-classes class-fields)))

(defonce metadata-atom (atom nil))

(defn load-metadata [aidbox-server]
  (or @metadata-atom
      (let [response @(http/get (str aidbox-server "/$metadata"))
            metadata (json/parse-string (:body response) keyword)]
        (reset! metadata-atom metadata))))

(defn generate-classes [zip metadata]
  (->> metadata :Entity
       (map (fn [entity]
              (when-not (primitive-types (first entity))
                (let [entity-name (entity-name (first entity))
                      entry-file (str "Resources/"  entity-name ".cs")
                      class-file (str "Resources\\" entity-name ".cs")]
                  (zip/with-entry zip entry-file
                    (generate-resource-class entity metadata))
                  class-file))))
       (filter identity)
       (doall)))

(defn generate-library [path]
  (let [uuid (u/uuid)]
    (zip/with-archive (zip (str path "AidboxSDK.zip"))
      (let [metadata (load-metadata "https://forsdk.aidbox.app")
            class-files (generate-classes zip metadata)]
        (zip/with-entry zip "AidboxSDK.csproj"
          (generate-project uuid class-files))
        (zip/with-entry zip "Properties/AssemblyInfo.cs"
          (generate-assembly-info uuid))))))
