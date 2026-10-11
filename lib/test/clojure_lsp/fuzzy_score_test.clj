(ns clojure-lsp.fuzzy-score-test
  (:require
   [anonimitoraf.clj-flx :as flx]
   [clojure-lsp.fuzzy-score :as score]
   [clojure.test :refer [deftest is testing]]))

(defn- words [n]
  (if (zero? n)
    [""]
    (for [prefix (words (dec n)) c "abc"] (str prefix c))))

(deftest exhaustive-equivalence-test
  (let [samples (mapcat words (range 1 5))]
    (is (every? (fn [[query candidate]]
                  (= (flx/score query candidate) (score/score query candidate)))
                (for [query samples candidate samples] [query candidate])))))

(deftest varied-equivalence-test
  (let [rng (java.util.Random. 91732)
        alphabet "aaabc-_./19"
        sample (fn [] (apply str (repeatedly (inc (.nextInt rng 24))
                                             #(.charAt alphabet (.nextInt rng (count alphabet))))))]
    (is (every? (fn [[query candidate]]
                  (= (flx/score query candidate) (score/score query candidate)))
                (repeatedly 5000 #(vector (sample) (sample))))))
  (testing "existing adjacent-order behavior is retained"
    (is (= 2 (score/score "abc" "bcab"))))
  (testing "Unicode, surrogate pairs and duplicate characters retain library behavior"
    (doseq [query ["é" "éé" "λ" "💜" "a💜" "aa"]
            candidate ["ééx" "aλé" "💜aa💜" "aaaa" "abc"]]
      (is (= (flx/score query candidate) (score/score query candidate))))))
