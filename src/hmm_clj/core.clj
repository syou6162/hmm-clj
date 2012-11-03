(ns hmm-clj.core
  (:use [hmm-clj.hmm])
  (:use [hmm-clj.mapping])
  (:use [clj-utils.core :only (flip)])
  (:use [clojure.string :only (split)]))

; (def BOS [(word-to-id "<S>") (pos-to-id "<S>")])
; (def EOS [(word-to-id "<E>") (pos-to-id "<E>")])
; (def UNK (word-to-id "<UNK>"))

(defn read-tagged-sequences [filename]
  (->> (slurp filename)
       (flip split #"\n")
       (map (fn [sent]
              (let [pairs (split sent #"\s")
                    result (map (fn [pair]
                                  (let [[word pos] (split pair #"/")]
                                    [(word-to-id word) (pos-to-id pos)]))
                                pairs)]
                result)))))

(defn get-tag-to-tag [sequences]
  (apply concat
         (map (fn [sent] (partition 2 1 (map second sent)))
              sequences)))

(defn table-to-prob [table delta]
  "Calculate the conditional probability
   for each column using Lidstone's law."
  (let [ncol (count table)]
    (into-array (vec (map
                      (fn [col-idx]
                        (let [col (nth table col-idx)
                              sum (+ (reduce + col) (* (count col) delta))]
                          (double-array (map #(/ (+ % delta) sum) col))))
                      (range ncol))))))

(defn -main [& args]
  (let [sequences (doall (read-tagged-sequences "pos-tag.txt"))
        tag-to-tag (get-tag-to-tag sequences)
        tag-to-word (vec (apply concat sequences))
        tag-to-tag-count (vec (reduce (fn [result [t1 t2]]
                                        (update-in result [t1 t2] inc))
                                      (vec (repeat (max-pos-id) (vec (repeat (max-pos-id) 0))))
                                      tag-to-tag))
        tag-to-word-count (vec (reduce (fn [result [w t]]
                                         (update-in result [t w] inc))
                                       (vec (repeat (max-pos-id) (vec (repeat (max-word-id) 0))))
                                       tag-to-word))
        tag-to-tag-prob (table-to-prob tag-to-tag-count 1.0)
        tag-to-word-prob (table-to-prob tag-to-word-count 1.0)
        n (max-pos-id)
        m (max-word-id)
        pi-sum (count (map (comp second first) sequences))
        pi (double-array (reduce
                          (fn [result [k v]] (assoc result k (/ v pi-sum)))
                          (vec (repeat n 0))
                          (frequencies (map (comp second first) sequences))))
        my-hmm (struct hmm n m pi tag-to-word-prob tag-to-tag-prob)
        observs (vec (map word-to-id ["美学" "的" "に" "プラットフォーム" "を"
                                      "横渡" "る" "一貫" "し" "た" "ＧＵＩ"]))
        [result _] (viterbi my-hmm observs)]
    (dorun (map (fn [[word-id pos-id]]
                  (println (id-to-word word-id) (id-to-pos pos-id)))
                (map vector observs result)))))
