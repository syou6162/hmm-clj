(ns hmm-clj.hmm)

;; References
;; 
;; Algorithm Implementation/Viterbi algorithm - Wikibooks, open books for an open world
;; http://en.wikibooks.org/wiki/Algorithm_Implementation/Viterbi_algorithm#Clojure_implementation
;; 
;; "A Revealing Introduction to Hidden Markov Models"
;; http://www.cs.sjsu.edu/faculty/stamp/RUA/HMM.pdf

(defstruct hmm :n :m :init-probs :emission-probs :state-transitions)

(defn make-hmm [{:keys [states, obs, init-probs, emission-probs, state-transitions]}]
  (struct-map hmm
    :n (count states)
    :m (count obs)
    :states states
    :obs obs
    :init-probs init-probs ;; n x n
    :emission-probs emission-probs ;; n x m
    :state-transitions state-transitions))

(defn indexed [s]
  (map vector (iterate inc 0) s))

(defn argmax [coll]
  "Return the maximum index and its value"
  (loop [s (indexed coll)
         max (first s)]
    (if (empty? s)
      max
      (let [[idx elt] (first s)
            [max-indx max-elt] max]
        (if (> elt max-elt)
          (recur (rest s) (first s))
          (recur (rest s) max))))))

(defn init-alphas [hmm obs]
  "alpha_0(i) = pi_i b_i(o_0) for all i"
  (map (fn [x]
         (* (get (:init-probs hmm) x) (get-in (:emission-probs hmm) [x obs])))
       (range (:n hmm))))

(defn scaling [coll]
  "a_i = a_i / (sum_j (a_j) for all i"
  (let [sum (reduce + coll)]
    (map (fn [item] (/ item sum)) coll)))

(defn forward
  ([hmm alphas obs]
     "alpha_t(i) for all i, and t is the index of obs"
     (let [alpha-t (map (fn [j]
                          (* (reduce (fn [sum i]
                                       (+ sum (* (nth alphas i) (get-in (:state-transitions hmm) [i j]))))
                                     0
                                     (range (:n hmm)))
                             (get-in (:emission-probs hmm) [j obs]))) (range (:n hmm)))]
       [(scaling alpha-t) (/ 1.0 (reduce + alpha-t))]))
  ([hmm observs]
     "Forward probability P(O) = sum_i alpha_T(i),
      which is marginalized out hidden variables."
     (loop [obs (rest observs)
            alphas (scaling (init-alphas hmm (first observs)))
            c [(/ 1.0 (reduce + (init-alphas hmm (first observs))))]]
       (if (empty? obs)
         (Math/exp (reduce (fn [sum c_i]
                             (- sum (Math/log c_i)))
                           0.0
                           c))
         (let [[alphas c-t] (forward hmm alphas (first obs))]
           (recur (rest obs)
                  alphas
                  (conj c c-t)))))))

(defn delta-max [hmm deltas obs]
  "delta_t(i) = log(b_i(o_t)) + max_j {delta_{t - 1}(j) + log(a_{j, i})} for all i"
  (map (fn [j]
         (+ (apply max (map (fn [i]
                              (+ (nth deltas i)
                                 (Math/log (get-in (:state-transitions hmm) [i j]))))
                            (range (:n hmm))))
            (Math/log (get-in (:emission-probs hmm) [j obs]))))
       (range (:n hmm))))

(defn backtrack [paths deltas]
  (loop [path (reverse paths)
         term (first (argmax deltas))
         backtrack []]
    (if (empty? path)
      (reverse (conj backtrack term))
      (recur (rest path) (nth (first path) term) (conj backtrack term)))))

(defn update-paths [hmm deltas]
  "psi_{t+1}(j) = argmax_i [delta_t(i) + log(a_{i, j})]"
  (map (fn [j]
         (first (argmax (map (fn [i]
                               (+ (nth deltas i)
                                  (Math/log (get-in (:state-transitions hmm) [i j]))))
                             (range (:n hmm))))))
       (range (:n hmm))))

(defn viterbi [hmm observs]
  "Return the pair of best path and its probability."
  (loop [obs (rest observs) ;; rest of the observations
         deltas (map #(Math/log %) (init-alphas hmm (first observs)))
         paths []]
    (if (empty? obs)
      [(backtrack paths deltas) (Math/exp (apply max deltas))]
      (recur (rest obs)
             (delta-max hmm deltas (first obs))
             (conj paths (update-paths hmm deltas))))))
