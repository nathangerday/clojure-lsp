(ns clojure-lsp.fuzzy-score
  (:require
   [anonimitoraf.clj-flx :as flx]))

(set! *warn-on-reflection* true)

(defn- ascii? [^String s]
  (loop [i 0]
    (or (= i (.length s))
        (and (< (int (.charAt s i)) 128) (recur (inc i))))))

(defn- matching? [^String query ^String candidate]
  (let [next-position (int-array 128)]
    (loop [i 0 previous-min -1]
      (if (= i (.length query))
        true
        (let [c (int (.charAt query i))
              first-position (.indexOf candidate c (aget next-position c))]
          ;; clj-flx consumes the first unused occurrence of each character and
          ;; compares each pair of adjacent occurrence sets. This deliberately
          ;; preserves that behavior; it is not a full subsequence predicate.
          (when (and (not= -1 first-position)
                     (< previous-min (.lastIndexOf candidate c)))
            (aset-int next-position c (inc first-position))
            (recur (inc i) first-position)))))))

(defn- longest-common-substring [^String query ^String candidate]
  (let [a (int-array (inc (.length query)))
        b (int-array (inc (.length query)))]
    (loop [i 0 previous a row b best 0]
      (if (= i (.length candidate))
        best
        (let [best (loop [j 0 best best]
                     (if (= j (.length query))
                       best
                       (let [value (if (= (.charAt candidate i) (.charAt query j))
                                     (inc (aget ^ints previous j))
                                     0)]
                         (aset-int ^ints row (inc j) value)
                         (recur (inc j) (max best value)))))]
          (recur (inc i) row previous (long best)))))))

(defn score
  "Allocation-bounded ASCII implementation of clj-flx's existing score and
  matching rules. Unicode retains the library's regex/character semantics."
  [^String query ^String candidate]
  {:pre [(seq query) (seq candidate)]}
  (if (and (ascii? query) (ascii? candidate))
    (when (matching? query candidate)
      (longest-common-substring query candidate))
    (flx/score query candidate)))
