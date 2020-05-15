(ns authorizer.main
  (:require [authorizer.core :refer [authorize!]]
            [camel-snake-kebab.core :as csk]
            [clojure.data.json :as json]
            [clojure.java.io :as io]))

;; Hello, Magit!
(defn json->hash-map [line]
  (json/read-str line :key-fn csk/->kebab-case-keyword))

(defn hash-map->json [result]
  (json/write-str result :key-fn csk/->camelCaseString))

(defn -main [& args]
  (doseq [line (line-seq (io/reader *in*))]
    (-> line
        json->hash-map
        authorize!
        hash-map->json
        println)))

