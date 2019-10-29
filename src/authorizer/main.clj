(ns authorizer.main
  (:require [clojure.data.json :as json]
            [camel-snake-kebab.core :as csk]
            [authorizer.core :refer [authorize!]]
            [clojure.java.io :as io]))

(defn json->hash-map [lines]
  (->> lines
       clojure.string/split-lines
       (map #(json/read-str % :key-fn csk/->kebab-case-keyword))))

(defn hash-map->json [result]
  (map #(json/write-str % :key-fn csk/->camelCaseString) result))

#_#_
(def lines "{\"account\": {\"activeCard\": true, \"availableLimit\": 100}}\n{\"transaction\": {\"merchant\": \"Burger King\", \"amount\": 20, \"time\": \"2019-02-13T10:00:00.000Z\"}}\n{\"transaction\": {\"merchant\": \"Habbib's\", \"amount\": 90, \"time\": \"2019-02-13T11:00:00.000Z\"}}")
(doseq [operation (json->hash-map lines)]
  (println (hash-map->json (authorize! operation))))

(defn -main [& args]
  (doseq [line (line-seq (io/reader *in*))]
    (-> line
        (json->hash-map)
        (authorize!)
        (hash-map->json)
        (println))))

