(ns clojure-lsp.java-completion-index
  (:require
   [clojure.string :as string]))

(set! *warn-on-reflection* true)

(defn- prefixes [{:keys [^String class]}]
  (when (seq class)
    (cond-> #{(.charAt class 0)}
      (string/starts-with? class "java.lang")
      (conj (.charAt class (inc (.lastIndexOf class ".")))))))

(defn- bucket-index [definitions]
  (reduce (fn [index element]
            (reduce #(update %1 %2 (fnil conj []) element) index (prefixes element)))
          {} definitions))

(defn refresh
  "Maintain prefix candidates only for changed Java class buckets. The index
  holds references to analysis elements, preserves duplicates, and is not
  serialized. Source edits and lazy Java member analysis leave it untouched."
  [db old-analysis changed-uris]
  (let [analysis (:analysis db)
        existing (:java-completion-index db)
        uris (if existing changed-uris (keys analysis))
        index (reduce
                (fn [index uri]
                  (let [old (get-in old-analysis [uri :java-class-definitions])
                        new (get-in analysis [uri :java-class-definitions])]
                    (if (and existing (identical? old new))
                      index
                      (let [old-buckets (bucket-index old)
                            new-buckets (bucket-index new)]
                        (reduce (fn [index prefix]
                                  (if-let [elements (get new-buckets prefix)]
                                    (assoc-in index [prefix uri] elements)
                                    (let [remaining (dissoc (get index prefix) uri)]
                                      (if (seq remaining)
                                        (assoc index prefix remaining)
                                        (dissoc index prefix)))))
                                index (into (set (keys old-buckets)) (keys new-buckets)))))))
                (or existing {}) uris)]
    (assoc db :java-completion-index index)))

(defn candidates [db prefix]
  (if-let [index (:java-completion-index db)]
    (mapcat val (get index prefix))
    ;; Standalone callers can supply an analysis snapshot without the DB update
    ;; pipeline. Retain their behavior without a process-global analysis cache.
    (mapcat (comp :java-class-definitions val) (:analysis db))))
