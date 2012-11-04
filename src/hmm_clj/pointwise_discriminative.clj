(ns hmm-clj.pointwise-discriminative
  (:use [hmm-clj.hmm])
  (:use [hmm-clj.mapping])
  (:use [clj-utils.core :only (flip split-with-ratio)])
  (:use [fobos-multiclass-clj.multiclass
         :only (multiclass-examples argmax-label get-models get-label-scores)])
  (:use [fobos-multiclass-clj.util :only (get-accuracy get-f-value)])
  (:use [clojure.string :only (split)]))

(defn mean [xs]
  (let [xs (filter #(not (Double/isNaN %)) xs)
        sum (reduce + xs)]
    (/ sum (count xs))))

(defstruct feature :type :str)
(def feature-names (atom []))

(defmacro def-feature-fn [feature-name args & body]
  `(let [name# (defn ~feature-name ~args ~@body)]
     (swap! feature-names conj name#)))

(defmacro def-around-feature-fn [feature-name idx]
  `(def-feature-fn ~feature-name [sentence#]
     (fn [target-idx#]
       (struct
        feature
        '~feature-name
        (get-in sentence# [(+ target-idx# ~idx) 0])))))

(def-around-feature-fn minus-three-feature -3)
(def-around-feature-fn minus-two-feature -2)
(def-around-feature-fn minus-one-feature -1)
(def-around-feature-fn zero-feature 0)
(def-around-feature-fn plus-one-feature 1)
(def-around-feature-fn plus-two-feature 2)
(def-around-feature-fn plus-three-feature 3)

(defn get-fv [sentence target-idx]
  (let [fv (->> (seq @feature-names)
                (map (fn [feature-fn] ((feature-fn sentence) target-idx)))
                (flatten)
                (filter (fn [fv] (not (nil? (:str fv))))))]
    (->> fv
         (map feature-to-id)
         (map #(vector % 1))
         (vec))))

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

(defn get-train-and-test-data [filename]
  (->> (read-tagged-sequences filename)
       (shuffle)
       (map (fn [raw-sent]
              (let [sent (vec raw-sent)
                    n (count sent)]
                (vec (map
                      (fn [target-idx]
                        [(second (nth sent target-idx)) (get-fv sent target-idx)])
                      (range n))))))
       (apply concat)
       (vec)))

(defn -main [& args]
  (let [all-data (get-train-and-test-data "wiki-sample.example")
        ratio 0.9
        [temp-training-examples test-examples] (split-with-ratio ratio all-data)
        training-examples (multiclass-examples temp-training-examples)
        iter 10
        eta 1.0
        lambda 0.001
        models (get-models training-examples iter eta lambda)
        gold (map first test-examples)
        prediction (map #(argmax-label models (second %)) test-examples)
        labels (set (map first training-examples))]
    (println (get-accuracy gold prediction)))
  (shutdown-agents))
