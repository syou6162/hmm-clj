(ns hmm-clj.hmm-test
  (:use clojure.test
        hmm-clj.hmm))

(deftest indexed-test
  (are [x y] (= x y)
       (indexed []) '()
       (indexed [:a :b :c]) '([0 :a] [1 :b] [2 :c])))

(deftest argmax-test
  (are [x y] (= x y)
       (argmax [1 2 3 9]) [3 9]
       (argmax [-1 -3 -2 3]) [3 3]))

(deftest scaling-test
  (are [x y] (= x y)

       (scaling (double-array '(0.3 0.2 0.5)))
       '(0.3 0.2 0.5)
       
       (scaling [1 2 3])
       '(1/6 1/3 1/2)))

;; References
;; 確率的言語モデル 第4章
;; 
;; 前向きアルゴリズム、Vitebiアルゴリズム
;; http://d.hatena.ne.jp/syou6162/20100708/1278577199

(def my-hmm1 (struct
              hmm
              5
              5
              (double-array [0.6, 0.4, 0.0, 0.0, 0.0])
              (into-array [(double-array [0.6, 0.1, 0.0, 0.0, 0.3])
                           (double-array [0.0, 0.0, 0.0, 1.0, 0.0])
                           (double-array [0.1, 0.2, 0.7, 0.0, 0.0])
                           (double-array [0.0, 0.0, 1.0, 0.0, 0.0])
                           (double-array [0.0, 0.0, 1.0, 0.0, 0.0])])
              (into-array [(double-array [0.3, 0.0, 0.4, 0.1, 0.2])
                           (double-array [0.7, 0.0, 0.0, 0.3, 0.0])
                           (double-array [0.3, 0.2, 0.1, 0.2, 0.2])
                           (double-array [0.5, 0.1, 0.0, 0.4, 0.0])
                           (double-array [0.6, 0.3, 0.0, 0.1, 0.0])])))

(deftest forward-test1
  (is (= (forward my-hmm1 [0 1 2 3 4])) 8.542799999999998E-4))

(deftest viterbi-test1
  (is (= (viterbi my-hmm1 [0 1 2 3 4])
         ['(0 2 4 1 0) 3.628800000000001E-4])))

;; http://ja.wikipedia.org/wiki/ビタビアルゴリズム

(def my-hmm2 (struct
              hmm
              2 3
              (double-array [0.6, 0.4])
              (into-array [(double-array [0.1 0.4 0.5])
                           (double-array [0.6 0.3 0.1])])
              (into-array [(double-array [0.7 0.3])
                           (double-array [0.4 0.6])])))

(deftest forward-test2
  (is (= (forward my-hmm2 [0 1 2])
         0.03361199999999998)))

(deftest viterbi-test2
  (is (= (viterbi my-hmm2 [0 1 2])
         ['(1 0 0) 0.013439999999999999])))
