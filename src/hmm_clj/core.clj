(ns hmm-clj.core
  (:use [hmm-clj.hmm])
  (:use [hmm-clj.mapping])
  (:use [clj-utils.core :only (flip)])
  (:use [clj-utils.evaluation :only (get-accuracy)])
  (:use [clojure.string :only (split)]))

(def BOS-word "<S>")
(def BOS-word-id (word-to-id BOS-word))
(def BOS-pos "<S>")
(def BOS-pos-id (pos-to-id BOS-pos))
(def BOS [BOS-word-id BOS-pos-id])

(def EOS-word "<E>")
(def EOS-word-id (word-to-id EOS-word))
(def EOS-pos "<E>")
(def EOS-pos-id (pos-to-id EOS-pos))
(def EOS [EOS-word-id EOS-pos-id])

(def UNK-word "<UNK>")
(def UNK-word-id (word-to-id UNK-word))

(defn read-tagged-sequences [filename]
  (->> (slurp filename)
       (flip split #"\n")
       (map (fn [sent]
              (let [pairs (split sent #"\s")
                    result (vec (map (fn [pair]
                                       (let [[word pos] (split pair #"/")]
                                         [(word-to-id word) (pos-to-id pos)]))
                                     pairs))]
                (concat [BOS] result [EOS]))))
       (vec)))

(defn read-test-sequences [filename max-word-id]
  (->> (slurp filename)
       (flip split #"\n")
       (map (fn [sent]
              (let [pairs (split sent #"\s")
                    result (vec (map (fn [pair]
                                       (let [[word pos] (split pair #"/")
                                             tmp-word-id (word-to-id word)
                                             word-id (if (>= tmp-word-id max-word-id)
                                                       UNK-word-id
                                                       tmp-word-id)]
                                         [word-id (pos-to-id pos)]))
                                     pairs))]
                (concat [BOS] result [EOS]))))
       (vec)))

(defn tag-to-tag [sequence]
  (partition 2 1 (map second sequence)))

(defn table-to-prob [table delta]
  "Calculate the conditional probability
   for each column using Lidstone's law."
  (let [ncol (count table)]
    (vec (map
          (fn [col-idx]
            (let [col (nth table col-idx)
                  sum (+ (reduce + col) (* (count col) delta))]
              (vec (map #(/ (+ % delta) sum) col))))
          (range ncol)))))

(defn inv-obj-to-obj [obj-to-obj]
  "[[:a 1] [:b 2]] => [[1 :a] [:2 b]]"
  (vec (map (comp vec reverse) obj-to-obj)))

(defn -main [& args]
  (let [sequences (doall (read-tagged-sequences "train.txt"))
        n (max-pos-id)
        m (max-word-id)
        tag-to-tag-seq (map tag-to-tag sequences)
        tag-to-word-seq (map inv-obj-to-obj sequences)
        tag-to-tag-count (reduce (fn [result [t1 t2]] (update-in result [t1 t2] inc))
                                 (vec (repeat n (vec (repeat n 0))))
                                 (apply concat tag-to-tag-seq))
        tag-to-word-count (reduce (fn [result [t w]] (update-in result [t w] inc))
                                  (vec (repeat n (vec (repeat m 0))))
                                  (apply concat tag-to-word-seq))
        tag-to-tag-prob (table-to-prob tag-to-tag-count 0.5)
        tag-to-word-prob (table-to-prob tag-to-word-count 0.01)
        pi-sum (count (map (comp second first) sequences))
        pi (reduce
            (fn [result [k v]] (assoc result k (/ v pi-sum)))
            (vec (repeat n 0))
            (frequencies (map (comp second first) sequences)))
        my-hmm (struct hmm n m pi tag-to-word-prob tag-to-tag-prob)]
    (let [test-sequences (read-test-sequences "test.txt" m)
          observs-seq (map (fn [seq] (map first seq)) test-sequences)
          gold-pos-tag-seq (map (fn [seq] (map second seq)) test-sequences)
          result (apply concat
                        (pmap (fn [[observs gold-pos-tag]]
                                (let [[viterbi-path _] (viterbi my-hmm observs)
                                      result (map (fn [[pos-id gold]]
                                                    [(id-to-pos pos-id) (id-to-pos gold)])
                                                  (map vector viterbi-path gold-pos-tag))]
                                  result))
                              (map vector observs-seq gold-pos-tag-seq)))]
      (println (get-accuracy (map first result) (map second result)))))
  (shutdown-agents))
