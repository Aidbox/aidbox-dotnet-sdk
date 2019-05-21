(ns sdk-generator.core
  (:require [sdk-generator.zip :as zip]
            [sdk-generator.utils :as u]
            [clojure.pprint :as pprint]
            [org.httpkit.client :as http]
            [cheshire.core :as json]))

(def project        (u/resource "project.tpl"))
(def assembly-info  (u/resource "assembly-info.tpl"))
(def resource-class (u/resource "resource-class.tpl"))
(def primitive-types {:unsignedInt "System.UInt64"
                      :unsignedint "System.UInt64"
                      :date "System.DateTime"
                      :instant "System.DateTime"
                      :time "System.DateTime"
                      :string "System.String"
                      :decimal "System.Decimal"})

(defn fmt [s & args]
  (apply pprint/cl-format true s args))

(defn generate-project [uuid files]
  (fmt project uuid files))

(defn generate-assembly-info [uuid]
  (fmt assembly-info uuid))

;; https://forsdk.aidbox.app/$metadata
(defn generate-resource-class [[id props]]
  (let [comment (or (:description props) "")
        modifiers (if (= (:type props) "abstract")
                    "abstract"
                    "partial")
        class-name (name id)
        base-class (case id
                     :Resource ""
                     (:DomainResource :Bundle :Parameters :Binary) " : Resource"
                     ": DomainResource")]
    (fmt resource-class comment modifiers class-name base-class)))

(defn find-resource-attributes [resource metadata]
  (->> metadata :Attribute
       (filter #(= (-> % second :resource :id) (name resource)))
       (map second)))

(defn load-metadata [zip aidbox-server]
  (let [response @(http/get (str aidbox-server "/$metadata"))
        metadata (json/parse-string (:body response) keyword)]
    (->> metadata :Entity
         (map (fn [entity]
                (when-not (primitive-types (first entity))
                  (let [entry-file (str "Resources/"  (name (first entity)) ".cs")
                        class-file (str "Resources\\" (name (first entity)) ".cs")]
                    (zip/with-entry zip entry-file
                      (generate-resource-class entity))
                    class-file))))
         (filter identity)
         (doall))))

(defn generate-library [path]
  (let [uuid (u/uuid)]
    (zip/with-archive (zip (str path "AidboxSDK.zip"))
      (let [class-files (load-metadata zip "https://forsdk.aidbox.app")]
        (zip/with-entry zip "AidboxSDK.csproj"
          (generate-project uuid class-files))
        (zip/with-entry zip "Properties/AssemblyInfo.cs"
          (generate-assembly-info uuid))))))
