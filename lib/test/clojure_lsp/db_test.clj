(ns clojure-lsp.db-test
  (:require
   [babashka.fs :as fs]
   [clojure-lsp.config :as config]
   [clojure-lsp.db :as db]
   [clojure.java.io :as io]
   [clojure.test :refer [deftest is testing]]
   [cognitect.transit :as transit]))

(defn ^:private temp-cache-file []
  (doto (java.io.File/createTempFile "clojure-lsp.db-test" ".transit.json")
    (.deleteOnExit)))

(deftest cache-analysis-round-trip-test
  (let [uri "file:///project/a.clj"
        jar-uri "zipfile:///foo.jar::foo/bar.clj"
        cache {:version db/version
               :project-root "/project"
               :classpath ["/project/src"]
               :analysis {uri {:var-definitions [{:uri uri :name 'foo :bucket :var-definitions}]
                               :var-usages [{:uri uri :name 'bar :bucket :var-usages}
                                            {:uri uri :name 'baz :bucket :var-usages}]}
                          jar-uri {:java-class-definitions [{:uri jar-uri :class "foo.Bar" :bucket :java-class-definitions}]}}
               :source-paths-checksums {"/project/src/a.clj" 123456}
               :kondo-findings {uri [{:uri uri :type :unused-namespace :level :warning}]}
               :dep-graph '{foo.bar {:uris #{"zipfile:///foo.jar::foo/bar.clj"}
                                     :internal? false
                                     :aliases {bar 1}}}
               :documents {uri {:namespaces #{'a} :internal? true :langs #{:clj}}
                           jar-uri {:namespaces #{'foo.bar} :internal? false :langs #{:clj}}}}
        cache-file (temp-cache-file)]
    (#'db/upsert-cache! cache cache-file)
    (testing "elements are written without the redundant :uri"
      (let [raw (with-open [is (io/input-stream cache-file)]
                  (transit/read (transit/reader is :json)))]
        (is (= [{:name 'foo :bucket :var-definitions}]
               (get-in raw [:analysis uri :var-definitions])))
        (is (= [{:class "foo.Bar" :bucket :java-class-definitions}]
               (get-in raw [:analysis jar-uri :java-class-definitions])))
        (testing "dep-graph, documents and source checksums are written untouched"
          (is (= (:dep-graph cache) (:dep-graph raw)))
          (is (= (:documents cache) (:documents raw)))
          (is (= (:source-paths-checksums cache) (:source-paths-checksums raw)))
          (is (= (:kondo-findings cache) (:kondo-findings raw))))))
    (testing "read cache restores elements :uri"
      (let [read-cache (#'db/read-cache cache-file)]
        (is (= cache read-cache))
        (testing "sharing the analysis key String instance"
          (doseq [[uri buckets] (:analysis read-cache)
                  [_bucket elements] buckets
                  element elements]
            (is (identical? uri (:uri element)))))))))

(deftest cache-edge-cases-test
  (let [cache-file (temp-cache-file)]
    (testing "cache without :analysis round-trips"
      (#'db/upsert-cache! {:version db/version :project-root "/project"} cache-file)
      (is (= {:version db/version :project-root "/project"}
             (#'db/read-cache cache-file))))
    (testing "cache with other version is ignored"
      (#'db/upsert-cache! {:version (dec db/version) :analysis {}} cache-file)
      (is (nil? (#'db/read-cache cache-file))))))

(deftest cache-write-failure-keeps-previous-cache-test
  (let [cache-file (temp-cache-file)
        good-cache {:version db/version :project-root "/project" :classpath ["/project/src"]}]
    (#'db/upsert-cache! good-cache cache-file)
    (testing "a write that fails mid-serialization does not truncate the previous cache"
      ;; functions are not transit-serializable, so this write fails
      (#'db/upsert-cache! (assoc good-cache :kondo-findings {"file:///a.clj" [{:linters vec}]})
                          cache-file)
      (is (= good-cache (#'db/read-cache cache-file)))
      (is (not (.exists (io/file (str cache-file ".tmp"))))))))

(deftest binary-cache-and-legacy-migration-test
  (let [dir (fs/create-temp-dir)
        root (str dir)
        legacy (io/file root "db.transit.json")
        binary (io/file root "db.transit.msgpack")
        uri "file:///project/a.clj"
        cache {:version db/version :project-root root
               :analysis {uri {:var-definitions [{:uri uri :name 'foo}]}}
               :documents {uri {:langs #{:clj :cljs}}}
               :dep-graph '{foo {:aliases {bar 2}}}}]
    (try
      (with-redefs [config/local-cache-dir (constantly (io/file root))]
        (#'db/upsert-cache! cache legacy)
        (testing "legacy cache remains usable before migration"
          (is (db/db-exists? {}))
          (is (= cache (db/read-local-cache root {})))
          (is (::db/legacy-cache? (meta (db/read-local-cache root {})))))
        (testing "a failed binary write preserves the legacy cache"
          (db/upsert-local-cache! (assoc cache :unserializable vec) {})
          (is (.exists legacy))
          (is (not (.exists binary)))
          (is (= cache (db/read-local-cache root {}))))
        (testing "successful migration preserves the data and removes legacy JSON"
          (db/upsert-local-cache! cache {})
          (is (.exists binary))
          (is (not (.exists legacy)))
          (is (= cache (db/read-local-cache root {})))
          (is (nil? (::db/legacy-cache? (meta (db/read-local-cache root {})))))
          (let [restored (db/read-local-cache root {})
                key-uri (first (keys (:analysis restored)))]
            (is (identical? key-uri (get-in restored [:analysis key-uri :var-definitions 0 :uri])))))
        (testing "a failed subsequent write preserves the previous binary cache"
          (db/upsert-local-cache! (assoc cache :unserializable vec) {})
          (is (= cache (db/read-local-cache root {})))
          (is (not (.exists (io/file (str binary ".tmp"))))))
        (testing "a damaged binary cache can fall back to a valid legacy cache"
          (#'db/upsert-cache! cache legacy)
          (spit binary "not a transit cache")
          (is (= cache (db/read-local-cache root {}))))
        (testing "removing the cache removes both formats"
          (db/remove-db! {})
          (is (not (db/db-exists? {})))
          (is (not (.exists legacy)))
          (is (not (.exists binary)))))
      (finally (fs/delete-tree dir)))))
