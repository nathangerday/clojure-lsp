(ns clojure-lsp.java-completion-index-test
  (:require
   [clojure-lsp.feature.completion :as completion]
   [clojure-lsp.java-completion-index :as index]
   [clojure-lsp.kondo :as kondo]
   [clojure.string :as string]
   [clojure.test :refer [deftest is testing]]))

(defn- items [db prefix]
  (vec (#'completion/with-java-definition-items
         #(string/starts-with? % prefix) prefix db)))

(deftest candidate-index-test
  (let [a {:class "java.lang.String"}
        b {:class "some.pkg.String"}
        c {:class "java.langx.Short"}
        analysis {"a" {:java-class-definitions [a a]}
                  "b" {:java-class-definitions [b c]}}
        db {:analysis analysis}
        indexed (index/refresh db {} (keys analysis))]
    (testing "qualified/short matches, duplicates and existing java.lang prefix semantics"
      (doseq [prefix ["j" "java.lang" "S" "Str" "s" "some" "missing" ""]]
        (is (= (items db prefix) (items indexed prefix)))))
    (testing "unrelated source edits and lazy member analysis reuse the same index"
      (let [source (kondo/db-with-analysis indexed {:analysis {"src" {:var-definitions [{:name 'foo}]}}})
            members (kondo/db-with-merged-analysis source {:analysis {"a" {:java-member-definitions [{:name "length"}]}}})]
        (is (identical? (:java-completion-index indexed) (:java-completion-index source)))
        (is (identical? (:java-completion-index source) (:java-completion-index members)))))
    (testing "replacement and deletion remove old candidates"
      (let [replacement (kondo/db-with-analysis indexed {:analysis {"a" {:java-class-definitions [{:class "other.New"}]}}})
            deleted (kondo/db-without-uris replacement ["b"])]
        (doseq [db [replacement deleted]
                prefix ["S" "j" "o" "s"]]
          (is (= (items (dissoc db :java-completion-index) prefix) (items db prefix))))))
    (testing "addition through the central analysis update"
      (let [updated (kondo/db-with-analysis indexed {:analysis {"z" {:java-class-definitions [{:class "java.lang.System"}]}}})]
        (is (= (items (dissoc updated :java-completion-index) "S") (items updated "S")))))))
