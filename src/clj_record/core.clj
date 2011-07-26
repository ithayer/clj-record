(ns clj-record.core
  (:require [clojure.java.jdbc          :as jdbc]
            [clojure.contrib.str-utils  :as str-utils])
  (:use (clj-record meta util callbacks)))


(defn table-name
  "Retrieves table-name from model-metadata."
  [model-name]
  (model-metadata-for model-name :table-name))

(defn set-table-name
  "Puts table-name into model metadata."
  [model-name tbl-name]
  (dosync (set-model-metadata-for model-name :table-name tbl-name)))

(defn set-db-spec [model-name db-spec]
  (dosync (set-model-metadata-for model-name :db-spec db-spec)))

(defn set-pk [model-name pk]
  (dosync (set-model-metadata-for model-name :pk pk)))

(defn to-conditions
  "Converts the given attribute map into a clojure.contrib.sql style 'where-params,'
  a vector containing a parameterized conditions string followed by ordered values for the parameters.
  Conditions will be ANDed together.
  Nil attributes will be turned into 'attr_name IS NULL' with no value in the vector."
  [attributes]
  ; XXX: Surely there's a better way.
  (let [[parameterized-conditions values] (reduce
      (fn [[parameterized-conditions values] [attribute value]]
       (cond
         (nil? value)
         [(conj parameterized-conditions (format "%s IS NULL" (name attribute))) values]
         (fn? value)
         (let [[new-condition new-values] (value attribute)]
           [(conj parameterized-conditions new-condition) (apply conj values new-values)])
         :else
         [(conj parameterized-conditions (format "%s = ?" (name attribute))) (conj values value)]))
      [[] []]
      attributes)]
    (apply vector (str-utils/str-join " AND " parameterized-conditions) values)))

(defmacro connected
  "Ensures that the body is run with a single DB connection.
  Doesn't create a new connection if there already is one.
  You're probably more interested in the 'transaction' macro."
  [db-spec & body]
  `(let [func# (fn [] ~@body)]
    (if (jdbc/find-connection)
      (func#)
      (jdbc/with-connection ~db-spec (func#)))))

(defmacro transaction
  "Runs body in a single DB transaction, first ensuring there's a connection."
  ([db-spec & body]
  `(connected ~db-spec
    (jdbc/transaction
      ~@body))))

(defmacro model-transaction
  "Runs body in a single DB transaction, first ensuring there's a connection. Note: body is a single list in this
instance."
  [& body]
  `(transaction (db-spec-for ~'model-name) ~@body))

(defn find-by-sql
  "Returns a vector of matching records.
  select-query-and-values should be something like
    [\"SELECT id, name FROM manufacturers WHERE id = ?\" 23]
  This allows the caller total control over the SELECT and FROM clauses, but note that callbacks are still run,
  so if you omit columns your callbacks will have to be written to tolerate incomplete records."
  [model-name select-query-and-values]
    (connected (db-spec-for model-name)
               (let [x (jdbc/with-query-results rows select-query-and-values
                         (doall (after-load model-name rows)))]
                 (println x)
                 x)))

(defn find-records
  "Returns a vector of matching records.
  Given a where-params vector, uses it as-is. (See clojure.contrib.jdbc/with-query-results.)
  Given a map of attribute-value pairs, uses to-conditions to convert to where-params."
  [model-name attributes-or-where-params]
  (let [[parameterized-where & values]
          (if (map? attributes-or-where-params)
            (to-conditions attributes-or-where-params)
            attributes-or-where-params)
          select-query (format "select * from %s where %s" (table-name model-name) parameterized-where)]
    (find-by-sql model-name (apply vector select-query values))))

(defn find-record
  "Returns the first matching record. This is just (first (find-records ...)).
  Note that at the moment there's no optimization to prevent the entire result set being read and converted into records."
  [model-name attributes-or-where-params]
  (first (find-records model-name attributes-or-where-params)))

(defn record-count
  "Returns the number of records that match, or the total number of records
  in the table." 
  ([model-name] (record-count model-name ["1=1"]))
  ([model-name attributes-or-where-params]
     (let [[parameterized-where & values]
           (if (map? attributes-or-where-params)
             (to-conditions attributes-or-where-params)
             attributes-or-where-params)
           select-query (format "select count(*) as count from %s where %s" (table-name model-name) parameterized-where)]
       (:count (first (find-by-sql model-name (apply vector select-query values)))))))

(defn get-record
  "Retrieves record by id, throwing if not found."
  [model-name id]
  (or (find-record model-name {(keyword (pk-for model-name)) id})
      (throw (IllegalArgumentException. "Record does not exist"))))

(defn- query-for-auto-id [model-name pk-name]
  "Executes a query to get the current value of a auto-incremented sequence."
  (jdbc/with-query-results rows [(id-query-for (db-spec-for model-name)
                                               (table-name model-name)
                                               pk-name)]
    (val (ffirst rows))))

(defn insert
  "Inserts a record populated with attributes and returns the generated id."
  [model-name attributes]
  (let [pk-name (pk-for model-name)]
    (transaction
     (db-spec-for model-name)
     (let [attributes (before-insert model-name (before-save model-name attributes))]
       (jdbc/insert-values (table-name model-name)
                           (keys attributes)
                           (vals attributes)))
     (let [id (or ((keyword pk-name) attributes) ;; id in attributes or query for it.
                  (query-for-auto-id model-name pk-name))]
       (after-save model-name
                   (after-insert model-name
                                 (assoc attributes (keyword pk-name) id)))
       id))))

(defn create
  "Inserts a record populated with attributes and returns it."
  [model-name attributes]
  (let [id (insert model-name attributes)]
    (connected (db-spec-for model-name)
      (get-record model-name id))))

(defn update
  "Updates by (partial-record :id), updating only those columns included in partial-record."
  [model-name partial-record]
  (connected
   (db-spec-for model-name)
   (let [pk-keyword (keyword (pk-for model-name))
         id (partial-record pk-keyword)
         partial-record (-> partial-record (run-callbacks model-name :before-save :before-update) (dissoc pk-keyword))]
      (jdbc/update-values (table-name model-name) [(str (pk-for model-name) " = ?") id] partial-record)
      (let [output-record (assoc partial-record pk-keyword id)]
        (after-save model-name (after-update model-name output-record))
        output-record))))

(defn destroy-record
  "Deletes by (record :id)."
  [model-name record]
  (connected (db-spec-for model-name)
    (before-destroy model-name record)
    (jdbc/delete-rows
     (table-name model-name)
     [(str (pk-for model-name) " = ?") ((keyword (pk-for model-name)) record)])
    (after-destroy model-name record)))

(defn destroy-records
  "Deletes all records matching (-> attributes to-conditions), 
  running before- and after-destroy callbacks on each matching record."
  [model-name attributes]
  (let [conditions (to-conditions attributes)
        model-table-name (table-name model-name)
        [parameterized-where & values] conditions
        select-query (format "select * from %s where %s" model-table-name parameterized-where)]
    (connected (db-spec-for model-name)
      (jdbc/with-query-results rows-to-delete (apply vector select-query values)
        (doseq [record rows-to-delete]
          (before-destroy model-name record))
        (jdbc/delete-rows model-table-name conditions)
        (doall
          (map #(after-destroy model-name %) rows-to-delete))))))

(defn delete-records
  "Deletes all records matching (-> attributes to-conditions) without running callbacks."
  [model-name attributes]
  (connected (db-spec-for model-name)
    (jdbc/delete-rows (table-name model-name) (to-conditions attributes))))

(defn- defs-from-option-groups [model-name option-groups]
  (reduce
    (fn [def-forms [option-group-name & options]]
      (let [option-ns (symbol (str "clj-record." (name option-group-name)))
            expand-init-option-fn (ns-resolve option-ns 'expand-init-option)]
        (if (nil? expand-init-option-fn)
          (throw (IllegalArgumentException. (format "%s/expand-init-option not defined" option-ns))))
        (into def-forms (map #(apply expand-init-option-fn model-name %) options))))
    []
    option-groups))

(defn- split-out-init-options [init-options]
  (loop [top-level-options {}
         remaining-options init-options]
    (if (keyword? (first remaining-options))
      (recur
        (assoc top-level-options (first remaining-options) (fnext remaining-options))
        (nnext remaining-options))
      [top-level-options remaining-options])))

(defmacro create-model [model-symbol & init-options]
  (let [model-name (str model-symbol)
        [top-level-options option-groups] (split-out-init-options init-options)
        tbl-name (or (top-level-options :table-name) (dashes-to-underscores (pluralize model-name)))
        pk-name  (or (top-level-options :pk) "id")
        optional-defs (defs-from-option-groups model-name option-groups)
        quoting-fns {:keyword (fn [x#] (println (str "x=" x#)) x#)
                     :entity (fn [x#] (println (str "y=" x#)) (str "\"" x# "\"")) }]
    `(do
       (init-model-metadata ~model-name)
       (set-db-spec ~model-name ~'db)
       (set-table-name ~model-name ~tbl-name)
       (set-pk ~model-name ~pk-name)
       (def ~model-symbol
         {:model ~model-name
          :table (table-name ~model-name)
          :metadata  (fn [& args#] (apply model-metadata-for ~model-name args#))
          :record-count (fn
                          ([] (record-count ~model-name))
                          ([attributes#] (record-count ~model-name attributes#)))
          :get-record (fn [id#]
                        (jdbc/with-naming-strategy
                          ~quoting-fns (get-record ~model-name id#)))
          :find-records (fn [attributes#]
                          (jdbc/with-naming-strategy
                            ~quoting-fns (find-records ~model-name attributes#)))
          :find-record (fn [attributes#]
                         (jdbc/with-naming-strategy
                           ~quoting-fns (find-record ~model-name attributes#)))
          :find-by-sql (fn [select-query-and-values#]
                         (jdbc/with-naming-strategy
                           ~quoting-fns
                           (find-by-sql ~model-name select-query-and-values#)))
          :create (fn [attributes#]
                    (jdbc/with-naming-strategy
                      ~quoting-fns
                      (create ~model-name attributes#)))
          :insert (fn [attributes#]
                    (jdbc/with-naming-strategy ~quoting-fns
                      (insert ~model-name attributes#)))
          :update (fn [attributes#]
                    (jdbc/with-naming-strategy
                      ~quoting-fns (update ~model-name attributes#)))
          :destroy-record (fn [record#]
                            (jdbc/with-naming-strategy
                              ~quoting-fns (destroy-record ~model-name record#)))
          :destroy-records (fn [attributes#]
                             (jdbc/with-naming-strategy
                               ~quoting-fns
                               (destroy-records ~model-name attributes#)))
          :delete-records (fn [attributes#]
                            (jdbc/with-naming-strategy
                              ~quoting-fns (delete-records ~model-name attributes#)))
          :validate (fn [record#]
                      (~'clj-record.validation/validate ~model-name record#))
          :after-destroy (fn [attributes#]
                           (after-destroy ~model-name attributes#))
          :after-insert (fn [attributes#]
                          (after-insert ~model-name attributes#))
          :after-load (fn [rows#]
                        (after-load ~model-name rows#))
          :after-save (fn [attributes#]
                        (after-save ~model-name attributes#))
          :after-update (fn [attributes#]
                          (after-update ~model-name attributes#))
          :after-validation (fn [attributes#]
                              (after-validation ~model-name attributes#))
          :before-destroy (fn [attributes#]
                            (before-destroy ~model-name attributes#))
          :before-insert (fn [attributes#]
                           (before-insert ~model-name attributes#))
          :before-save (fn [attributes#]
                         (before-save ~model-name attributes#))
          :before-update (fn [attributes#]
                           (before-update ~model-name attributes#))
          :before-validation (fn [attributes#]
                               (before-validation ~model-name attributes#))})
         ~@optional-defs)))