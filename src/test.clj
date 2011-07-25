(ns clj-record.test2
  (:require clj-record.boot))

(def db 
  {:classname   "org.postgresql.Driver"
   :subprotocol "postgresql"
   :user        "ignacio"
   :password    "postgrespass"
   :subname     (format "//%s:%s/%s" "localhost" "5432" "rfz-1") })

(defn truthy-get-time [sql-timestamp]
  (when sql-timestamp
    (.getTime sql-timestamp)))
  
(defn sql-timestamps-to-ms [record & ks]
  (into record (map (fn [k] [k (truthy-get-time (record k))]) ks)))

(clj-record.core/init-model
 :table-name "rfz_yserviceaccount"
 :pk "username"
 (:callbacks
  (:after-load #(sql-timestamps-to-ms % :updated))))