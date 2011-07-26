(ns clj-record.associations
  (:require [clojure.java.jdbc          :as jdbc])
  (:use clj-record.meta
        clj-record.util))


(defn expand-init-option
  "Called via init-model when an :associations option group is encountered.
  Options are alternating key/value pairs."
  [model-name association-type-sym association-name & options]
  (let [assoc-fn (ns-resolve 'clj-record.associations association-type-sym)]
    (apply assoc-fn model-name association-name options)))

(def quoting-fns {:keyword (fn [x#] x#)
                  :entity (fn [x#] (str "\"" x# "\""))})

(defn has-many
  "Defines an association to a model whose name is infered by singularizing association-name.
  In ns foo's init-model, (has-many bars) will define find-bars and destroy-bars functions in foo,
  each of which take a foo record and find or destroy bars by {:foo_id (record :id)}.

  Options are alternating key/value pairs. Supported options:

    :fk foreign_key_col_name
    :model target-model-name
  
  Called indirectly via clj-record.core/init-model."
  [model-name association-name & options]
  (let [opts (apply hash-map options)
        associated-model-name (str (or (:model opts) (singularize (name association-name))))
        foreign-key-attribute (keyword (or (:fk opts) (str (dashes-to-underscores model-name) "_" (pk-for model-name))))
        find-fn-name (keyword (str "find-" association-name))
        destroy-fn-name (keyword (str "destroy-" association-name))]
    `{~find-fn-name (fn  [record#]
                      (jdbc/with-naming-strategy
                        quoting-fns
                        (clj-record.core/find-records
                         ~associated-model-name {~foreign-key-attribute (record# (keyword (~pk-for ~model-name)))})))
      ~destroy-fn-name (fn [record#]
                         (jdbc/with-naming-strategy
                           quoting-fns
                           (clj-record.core/destroy-records ~associated-model-name {~foreign-key-attribute (record# (keyword (~pk-for ~associated-model-name)))})))}))

(defn belongs-to
  "Defines an association to a model named association-name.
  In ns bar's init-model, (belongs-to foo) will define find-foo in bar.

  Options are alternating key/value pairs. Supported options:

    :fk foreign_key_col_name
    :model target-model-name
  
  If model is specified and fk is not, fk name is inferred from the
  association name. For example,

    (belongs-to mother :model person)

  will assume the foreign key is mother_id.

  Called indirectly via clj-record.core/init-model."
  [model-name association-name & options]
  (let [opts (apply hash-map options)
        associated-model-name (str (or (:model opts) association-name))
        find-fn-name (keyword (str "find-" association-name))
        foreign-key-attribute (keyword (or
                                        (:fk opts)
                                        (str (dashes-to-underscores (str association-name)) "_" (pk-for associated-model-name))))]
    `{~find-fn-name (fn [record#]
                      (jdbc/with-naming-strategy
                        quoting-fns
                        (clj-record.core/get-record ~associated-model-name (~foreign-key-attribute record#))))}))