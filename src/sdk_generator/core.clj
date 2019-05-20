(ns sdk-generator.core
  (:require [sdk-generator.zip :as zip]
            [sdk-generator.utils :as u]
            [clojure.pprint :as pprint]))

(def project       (u/resource "project.tpl"))
(def assembly-info (u/resource "assembly-info.tpl"))

(defn fmt [s & args]
  (apply pprint/cl-format true s args))

(defn generate-project [uuid & files]
  (fmt project uuid files))

(defn generate-assembly-info [uuid]
  (fmt assembly-info uuid))

(defn generate-stub-class []
  (fmt "using System;

namespace AidboxSDK
{
    public class Test
    {
        public static void Main()
        {
            Console.WriteLine(\"Hello, World!\");
        }
    }
}"
       ))

(defn generate-library [path]
  (let [uuid (u/uuid)]
    (zip/with-archive (zip (str path "AidboxSDK.zip"))
      (zip/with-entry zip "Test.cs"
        (generate-stub-class))
      (zip/with-entry zip "AidboxSDK.csproj"
        (generate-project uuid "Test.cs"))
      (zip/with-entry zip "Properties/AssemblyInfo.cs"
        (generate-assembly-info uuid)))))
