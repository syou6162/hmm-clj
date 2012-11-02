(ns hmm-clj.hmm-test
  (:use clojure.test
        hmm-clj.hmm))

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

(let [my-hmm (struct
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
                           (double-array [0.6, 0.3, 0.0, 0.1, 0.0])]))]
  (deftest forward-test
    (is (= (forward my-hmm [0 1 2 3 4])) 8.542799999999998E-4))
  (deftest viterbi-test
    (is (= (viterbi my-hmm [0 1 2 3 4])
           ['(0 2 0 1 0) 3.628800000000001E-4]))))
