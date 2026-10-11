(ns clojure-lsp.startup-test
  (:require
   [babashka.fs :as fs]
   [clojure-lsp.classpath :as classpath]
   [clojure-lsp.clj-depend :as lsp.depend]
   [clojure-lsp.config :as config]
   [clojure-lsp.db :as db]
   [clojure-lsp.dep-graph :as dep-graph]
   [clojure-lsp.feature.completion :as completion]
   [clojure-lsp.feature.diagnostics.built-in :as f.diagnostics.built-in]
   [clojure-lsp.feature.diagnostics.custom :as f.diagnostics.custom]
   [clojure-lsp.kondo :as lsp.kondo]
   [clojure-lsp.settings :as lsp.settings]
   [clojure-lsp.shared :as shared]
   [clojure-lsp.startup :as startup]
   [clojure-lsp.test-helper.internal :as h]
   [clojure.java.io :as io]
   [clojure.string :as string]
   [clojure.test :refer [are deftest is testing]]))

(deftest cache-safe-findings-test
  (testing "nil findings stay nil"
    (is (nil? (#'startup/cache-safe-findings nil))))
  (testing "finding entries holding non-serializable values are dropped, plain data is kept"
    ;; e.g. metabase hooks reg-finding! with the node meta, which includes
    ;; :clj-kondo/ignore holding a rewrite-clj node whose :seq-fn is clojure.core/vec
    (is (= {"file:///a.clj" [{:type :some-custom-linter :row 1 :message "boom"}
                             {:type :unused-namespace :langs '() :level :warning}]}
           (#'startup/cache-safe-findings
            {"file:///a.clj" [{:type :some-custom-linter :row 1 :message "boom"
                               :clj-kondo/ignore {:linters [{:tag :vector :seq-fn vec}]}}
                              {:type :unused-namespace :langs '() :level :warning}]})))))

(deftest consider-local-db-cache?-test
  (are [session-analysis-type cache-analysis-type result]
       (= result
          (#'startup/consider-local-db-cache?
           {:project-analysis-type session-analysis-type}
           {:project-analysis-type cache-analysis-type}))
    :project-and-full-dependencies :project-and-full-dependencies true
    :project-and-full-dependencies :project-and-shallow-analysis false
    :project-and-full-dependencies :project-only false

    :project-and-shallow-analysis :project-and-full-dependencies true
    :project-and-shallow-analysis :project-and-shallow-analysis true
    :project-and-shallow-analysis :project-only false

    :project-only :project-and-full-dependencies true
    :project-only :project-and-shallow-analysis true
    :project-only :project-only true))

(deftest load-db-cache!-test
  (let [uri "zipfile:///foo.jar::foo/bar.clj"
        analysis {uri {:var-definitions [{:uri uri :name 'foo :bucket :var-definitions}]}}]
    (testing "reuses the cached dep-graph and documents instead of replaying refresh-analysis"
      (let [dep-graph '{foo.bar {:uris #{"zipfile:///foo.jar::foo/bar.clj"} :internal? false}}
            documents {uri {:namespaces #{'foo.bar} :internal? false :langs #{:clj}}}]
        (with-redefs [db/read-local-cache (constantly {:project-analysis-type :project-and-full-dependencies
                                                       :analysis analysis
                                                       :dep-graph dep-graph
                                                       :documents documents})
                      lsp.kondo/canonicalize-java-analysis identity
                      dep-graph/refresh-analysis (fn [& _] (throw (ex-info "should not rebuild dep-graph" {})))]
          (let [db* (atom {:project-analysis-type :project-and-full-dependencies})]
            (#'startup/load-db-cache! "/project" db*)
            (is (= dep-graph (:dep-graph @db*)))
            (is (= documents (:documents @db*)))
            (is (= analysis (:analysis @db*)))))))
    (testing "rebuilds the dep-graph when the cache has none (older cache format)"
      (with-redefs [db/read-local-cache (constantly {:project-analysis-type :project-and-full-dependencies
                                                     :analysis analysis})
                    lsp.kondo/canonicalize-java-analysis identity
                    dep-graph/refresh-analysis (fn [db _old _new _internal?] (assoc db :rebuilt? true))]
        (let [db* (atom {:project-analysis-type :project-and-full-dependencies})]
          (#'startup/load-db-cache! "/project" db*)
          (is (true? (:rebuilt? @db*)))
          (is (= analysis (:analysis @db*))))))))

(deftest dependency-scheme-change-resets-java-candidates-test
  (h/reset-components!)
  (let [components (h/components)
        db* (:db* components)
        old-uri "jar:file:///removed.jar!/Gone.class"
        new-uri "zipfile:///current.jar::New.class"
        cache {:project-analysis-type :project-and-full-dependencies
               :dependency-scheme "jar"
               :analysis {old-uri {:java-class-definitions [{:class "review.Gone" :uri old-uri}]}}
               :dep-graph {} :documents {} :classpath []}]
    (swap! db* assoc :project-analysis-type :project-and-full-dependencies)
    (with-redefs [config/resolve-for-root (fn [_] {})
                  startup/publish-task-progress (fn [& _])
                  startup/ensure-kondo-config-dir-exists! (fn [& _])
                  db/read-local-cache (fn [& _] cache)
                  classpath/project-specs->hash (fn [& _] "hash")
                  config/settings-hash (fn [& _] "hash")
                  lsp.kondo/config-hash (fn [& _] "hash")
                  classpath/scan-classpath! (fn [& _] [])
                  startup/copy-configs-from-classpath! (fn [& _])
                  startup/analyze-external-classpath! (fn [& _]
                                                        (swap! db* lsp.kondo/db-with-analysis
                                                               {:analysis {new-uri {:java-class-definitions [{:class "review.New" :uri new-uri}]}}
                                                                :external? true}))
                  startup/analyze-source-paths! (fn [& _] true)
                  startup/upsert-db-cache! (fn [& _])]
      (startup/initialize-project "file:///review-project" {} {:dependency-scheme "zipfile" :source-paths []} {} nil components))
    (let [items (fn [state] (vec (#'completion/with-java-definition-items
                                  #(string/starts-with? % "review.") "review." state)))]
      (is (= #{new-uri} (set (keys (:analysis @db*)))))
      (is (= ["review.New"] (mapv :label (items @db*))))
      (is (= (items (dissoc @db* :java-completion-index)) (items @db*)))))
  (h/reset-components!))

(deftest db-without-uris-test
  (let [uri "file:///a.clj"
        kept "file:///b.clj"
        db {:analysis {uri {:var-definitions [{:name 'a}]} kept {:var-definitions [{:name 'b}]}}
            :documents {uri {:internal? true} kept {:internal? true}}
            :dep-graph '{a {:uris #{"file:///a.clj"}} b {:uris #{"file:///b.clj"}}}
            :diagnostics {:clj-kondo {uri [{:code :x}] kept [{:code :y}]}}}
        result (lsp.kondo/db-without-uris db [uri])]
    (is (nil? (get-in result [:analysis uri])))
    (is (nil? (get-in result [:documents uri])))
    (is (nil? (get-in result [:diagnostics :clj-kondo uri])))
    (testing "other uris are kept"
      (is (some? (get-in result [:analysis kept])))
      (is (some? (get-in result [:diagnostics :clj-kondo kept]))))))

(deftest analyze-source-paths!-incremental-test
  (let [dir (str (fs/create-temp-dir))
        a-file (io/file dir "a.clj")
        b-file (io/file dir "b.clj")
        _ (spit a-file "(ns a)")
        _ (spit b-file "(ns b)")
        a-path (.getCanonicalPath a-file)
        b-path (.getCanonicalPath b-file)
        c-path (.getCanonicalPath (io/file dir "c.clj")) ;; never created => a deleted file
        analyzed (atom nil)
        evicted (atom nil)]
    (with-redefs [lsp.kondo/run-kondo-on-paths! (fn [paths _ _ _] (reset! analyzed (set paths)) {})
                  lsp.kondo/db-without-uris (fn [db uris] (reset! evicted (set uris)) db)
                  lsp.kondo/db-with-results (fn [db _] db)
                  lsp.depend/analyze-paths! (fn [_ _] {})
                  lsp.depend/db-with-results (fn [db _] db)
                  f.diagnostics.built-in/db-with-results (fn [db _] db)
                  f.diagnostics.custom/db-with-results (fn [db _] db)
                  lsp.settings/all (constantly {})
                  shared/filename->uri (fn [f _] (str "file://" f))]
      (testing "warm: re-analyzes only changed/added files and evicts deleted"
        (reset! analyzed nil)
        (reset! evicted nil)
        (let [db* (atom {:source-paths-checksums {a-path (.lastModified a-file)
                                                  b-path 0
                                                  c-path 123}})
              changed? (#'startup/analyze-source-paths! [dir] db* (fn [_]) false)]
          (is (= #{b-path} @analyzed))
          (is (= #{(str "file://" c-path)} @evicted))
          (is (true? changed?))
          (is (= {a-path (.lastModified a-file) b-path (.lastModified b-file)}
                 (:source-paths-checksums @db*)))))
      (testing "warm: nothing changed => no analysis, no eviction, returns false"
        (reset! analyzed nil)
        (reset! evicted nil)
        (let [db* (atom {:source-paths-checksums {a-path (.lastModified a-file)
                                                  b-path (.lastModified b-file)}})
              changed? (#'startup/analyze-source-paths! [dir] db* (fn [_]) false)]
          (is (nil? @analyzed))
          (is (= #{} @evicted))
          (is (false? changed?))))
      (testing "force-all (cold): analyzes every current source file"
        (reset! analyzed nil)
        (reset! evicted nil)
        (let [db* (atom {})
              changed? (#'startup/analyze-source-paths! [dir] db* (fn [_]) true)]
          (is (= #{a-path b-path} @analyzed))
          (is (= #{} @evicted))
          (is (true? changed?)))))))

(deftest unchanged-startup-reuses-built-in-diagnostics-test
  (let [dir (fs/create-temp-dir)
        file (io/file (str dir) "a.clj")
        path (.getCanonicalPath file)
        uri (str "file://" path)
        settings* (atom {:linters {:clojure-lsp/unused-public-var {:level :info}}})
        built-in-calls* (atom 0)
        custom-calls* (atom 0)
        db* (atom {})]
    (try
      (spit file "(ns a)")
      (with-redefs [lsp.settings/all (fn [_] @settings*)
                    lsp.kondo/run-kondo-on-paths! (fn [& _] {})
                    lsp.kondo/db-without-uris (fn [db _] db)
                    lsp.kondo/db-with-results (fn [db _] db)
                    lsp.depend/analyze-paths! (fn [& _] {})
                    lsp.depend/db-with-results (fn [db _] db)
                    f.diagnostics.built-in/analyze-paths! (fn [& _]
                                                            (swap! built-in-calls* inc)
                                                            {uri [{:message "Unused public var" :severity 3}]})
                    f.diagnostics.custom/analyze-paths! (fn [& _]
                                                          (swap! custom-calls* inc)
                                                          {})]
        (let [analyze #(do (#'startup/analyze-source-paths! [(str dir)] db* (fn [_]) %) @db*)]
          (testing "the first run computes diagnostics; unchanged startup restores the same diagnostics"
            (let [first-db (analyze false)
                  warm-db (analyze false)]
              (is (= 1 @built-in-calls*))
              (is (= (get-in first-db [:diagnostics :built-in])
                     (get-in warm-db [:diagnostics :built-in])))
              (is (= 2 @custom-calls*))))
          (testing "changing client/project settings recomputes diagnostics"
            (swap! settings* assoc-in [:linters :clojure-lsp/unused-public-var :level] :off)
            (analyze false)
            (is (= 2 @built-in-calls*)))
          (testing "an incompatible cached linter version recomputes diagnostics"
            (swap! db* assoc-in [:startup-built-in-lint-cache :version] 0)
            (analyze false)
            (is (= 3 @built-in-calls*)))
          (testing "a changed, added or deleted source file recomputes project diagnostics"
            (.setLastModified file (+ 3000 (.lastModified file)))
            (analyze false)
            (is (= 4 @built-in-calls*))
            (let [added (io/file (str dir) "b.clj")]
              (spit added "(ns b)")
              (analyze false)
              (is (= 5 @built-in-calls*))
              (io/delete-file added)
              (analyze false)
              (is (= 6 @built-in-calls*))))
          (testing "invalidated dependency analysis forces a full linter run"
            (analyze true)
            (is (= 7 @built-in-calls*)))))
      (finally (fs/delete-tree dir)))))

(deftest unchanged-startup-reuses-copied-kondo-configs-test
  (let [dir (fs/create-temp-dir)
        root (str dir)
        config-dir (io/file root ".clj-kondo")
        config-file (io/file config-dir "config.edn")
        copied-file (io/file config-dir "vendor" "config.edn")
        jar (io/file root "dependency.jar")
        source-dir (io/file root "src")
        exported (io/file source-dir "clj-kondo.exports" "local" "config.edn")
        classpath [(.getPath jar) (.getPath source-dir)]
        calls* (atom [])
        db* (atom {:project-root-uri (str "file://" root)})]
    (try
      (io/make-parents config-file)
      (spit config-file "{}")
      (spit jar "dependency")
      (io/make-parents exported)
      (spit exported "{}")
      (with-redefs [lsp.kondo/config-hash (fn [_] (slurp config-file))
                    lsp.kondo/run-kondo-copy-configs! (fn [paths _]
                                                        (swap! calls* conj paths)
                                                        (when (seq paths)
                                                          (io/make-parents copied-file)
                                                          (spit copied-file "{}"))
                                                        {:config {:linters {:unused-binding {:level :warning}}}})]
        (let [copy-configs #(#'startup/copy-configs-from-classpath! classpath {} db*)]
          (testing "imports once, then resolves current config without rescanning archives"
            (copy-configs)
            (copy-configs)
            (is (= [classpath []] @calls*))
            (is (= :warning (get-in @db* [:kondo-config :linters :unused-binding :level]))))
          (testing "clj-kondo's analysis cache does not invalidate copied configs"
            (let [cache-file (io/file config-dir ".cache" "analysis.json")]
              (io/make-parents cache-file)
              (spit cache-file "changed")
              (copy-configs)
              (is (= [] (last @calls*)))))
          (testing "changed dependency archive triggers import again"
            (.setLastModified jar (+ 3000 (.lastModified jar)))
            (copy-configs)
            (is (= classpath (last @calls*))))
          (testing "new exports in a classpath directory trigger import again"
            (let [added (io/file (.getParentFile exported) "hook.clj")]
              (spit added "(ns hook)")
              (copy-configs)
              (is (= classpath (last @calls*)))
              (io/delete-file added)
              (copy-configs)
              (is (= classpath (last @calls*)))))
          (testing "changed local config and removed copied config trigger import again"
            (spit config-file "{:linters {}}")
            (copy-configs)
            (is (= classpath (last @calls*)))
            (io/delete-file copied-file)
            (copy-configs)
            (is (= classpath (last @calls*))))
          (testing "disabled copying and namespaces-only mode don't import configs"
            (let [calls @calls*]
              (#'startup/copy-configs-from-classpath! classpath {:copy-kondo-configs? false} db*)
              (swap! db* assoc :project-analysis-type :project-namespaces-only)
              (copy-configs)
              (is (= calls @calls*))))))
      (finally (fs/delete-tree dir)))))
