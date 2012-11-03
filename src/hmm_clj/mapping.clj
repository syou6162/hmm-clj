(ns hmm-clj.mapping
  (:use [clj-utils.io :only (serialize deserialize)]))

(defmacro def-obj-and-id-mapping [obj-name]
  "Define the functions related to converting object <=> id."
  (let [obj-to-id (symbol (str obj-name "-to-id"))
        id-to-obj (symbol (str "id-to-" obj-name))
        save-obj-to-id (symbol (str "save-" obj-name "-to-id"))
        load-obj-to-id (symbol (str "load-" obj-name "-to-id"))
        clear-obj-mapping! (symbol (str "clear-" obj-name "-mapping!"))
        max-obj-id (symbol (str "max-" obj-name "-id"))]
    `(let [obj-to-id-mapping# (atom {})
           id-to-obj-mapping# (atom [])]
       (do
         (defn ~obj-to-id [obj#]
           (let [v# (get @obj-to-id-mapping# obj#)
                 max-id# (count @obj-to-id-mapping#)]
             (if v#
               v#
               (do (swap! obj-to-id-mapping# assoc obj# max-id#)
                   (swap! id-to-obj-mapping# conj obj#)
                   max-id#))))
         (defn ~id-to-obj [id#]
           (nth @id-to-obj-mapping# id#))
         (defn ~save-obj-to-id [filename#]
           (serialize {:obj-to-id-mapping @obj-to-id-mapping#
                       :id-to-obj-mapping @id-to-obj-mapping#}
                      filename#))
         (defn ~load-obj-to-id [filename#]
           (let [maps# (deserialize filename#)]
             (do (reset! obj-to-id-mapping# (:obj-to-id-mapping maps#))
                 (reset! id-to-obj-mapping# (:id-to-obj-mapping maps#)))))
         (defn ~clear-obj-mapping! []
           (do (reset! obj-to-id-mapping# {})
               (reset! id-to-obj-mapping# [])))
         (defn ~max-obj-id []
           (count @obj-to-id-mapping#))))))

(def-obj-and-id-mapping word)
(def-obj-and-id-mapping pos)
