(ns clojure-lsp.feature.file-management-test
  (:require
   [babashka.fs :as fs]
   [clojure-lsp.clj-depend :as depend]
   [clojure-lsp.feature.diagnostics :as diagnostics]
   [clojure-lsp.feature.diagnostics.built-in :as built-in]
   [clojure-lsp.feature.diagnostics.custom :as custom]
   [clojure-lsp.feature.file-management :as f.file-management]
   [clojure-lsp.handlers :as handlers]
   [clojure-lsp.kondo :as kondo]
   [clojure-lsp.parser :as parser]
   [clojure-lsp.producer :as producer]
   [clojure-lsp.shared :as shared]
   [clojure-lsp.test-helper.internal :as h]
   [clojure.core.async :as async]
   [clojure.test :refer [are deftest is testing]]
   [medley.core :as medley]
   [rewrite-clj.zip :as z]))

(h/reset-components-before-test)

(deftest analysis-retry-rejects-previous-document-sessions
  (doseq [[old-version new-version] [[100 0] [7 7]]]
    (h/reset-components!)
    (let [uri h/default-uri
          db* (h/db*)
          text "[1]"
          old-session (get-in (f.file-management/load-document! uri text db*)
                              [:documents uri :open-session])
          publications* (atom 0)]
      (swap! db* assoc-in [:documents uri :v] old-version)
      (with-redefs [kondo/run-kondo-on-text! (fn [_ _ _]
                                               ;; Reopen with identical text while old analysis is running.
                                               (f.file-management/load-document! uri text db*)
                                               (swap! db* assoc-in [:documents uri :v] new-version)
                                               {:analysis {uri {:var-definitions [{:name 'stale}]}}
                                                :findings {uri []}})
                    depend/analyze-uri! (fn [_ _] nil)
                    built-in/analyze-uri! (fn [_ _] {})
                    custom/analyze-uri! (fn [_ _] {})
                    diagnostics/publish-diagnostics! (fn [_ _] (swap! publications* inc))
                    f.file-management/notify-references (fn [& _])]
        (f.file-management/analyze-changes {:uri uri :text text :version old-version :open-session old-session}
                                           (h/components)))
      (is (not= old-session (get-in @db* [:documents uri :open-session])))
      (is (= new-version (get-in @db* [:documents uri :v])))
      (is (= -1 (get-in @db* [:documents uri :analyzed-version])))
      (is (empty? (get-in @db* [:analysis uri :var-definitions])))
      (is (zero? @publications*))
      (is (false? (#'handlers/caught-up? @db* uri (inc new-version)))))))

(deftest analysis-retry-reuses-only-unchanged-inputs
  (doseq [[label update-db expected-runs]
          [["unrelated bookkeeping" #(assoc % :progress :updated) 1]
           ["another document edit" #(assoc-in % [:documents "other" :text] "[2]") 1]
           ["settings change" #(assoc-in % [:settings :analysis :keywords] false) 2]
           ["dependency analysis change" #(assoc-in % [:analysis "other" :var-definitions] [{:name 'changed}]) 2]]]
    (testing label
      (h/reset-components!)
      (let [uri h/default-uri
            db* (h/db*)
            runs* (atom 0)
            depend-runs* (atom 0)
            diagnostics-states* (atom [])]
        (swap! db* assoc-in [:documents uri] {:text "[1]" :v 1})
        (with-redefs [kondo/run-kondo-on-text! (fn [_ _ snapshot*]
                                                 (when (= 1 (swap! runs* inc))
                                                   (is (= "[1]" (get-in @snapshot* [:documents uri :text])))
                                                   (swap! db* update-db))
                                                 {:analysis {uri {}} :findings {uri []}})
                      depend/analyze-uri! (fn [_ _] (swap! depend-runs* inc) nil)
                      built-in/analyze-uri! (fn [_ state]
                                              (swap! diagnostics-states* conj (:progress state))
                                              {})
                      custom/analyze-uri! (fn [_ _] {})
                      diagnostics/publish-diagnostics! (fn [_ _])
                      f.file-management/notify-references (fn [& _])]
          (f.file-management/analyze-changes {:uri uri :text "[1]" :version 1} (h/components)))
        (is (= expected-runs @runs*))
        (is (= expected-runs @depend-runs*))
        (is (= (if (= label "unrelated bookkeeping") 1 2)
               (count @diagnostics-states*)))
        (is (= 1 (get-in @db* [:documents uri :analyzed-version])))
        (when (= label "unrelated bookkeeping")
          (is (= [nil] @diagnostics-states*))
          (is (= :updated (:progress @db*))))))))

(deftest analysis-retry-abandons-stale-text
  (doseq [new-version [0 2]]
    (h/reset-components!)
    (let [uri h/default-uri
          db* (h/db*)
          published* (atom 0)]
      (swap! db* assoc-in [:documents uri] {:text "[1]" :v 0 :analyzed-version -1})
      (with-redefs [kondo/run-kondo-on-text! (fn [& _]
                                               (swap! db* assoc-in [:documents uri] {:text "[2]" :v new-version :analyzed-version -1})
                                               {:analysis {uri {}} :findings {uri []}})
                    depend/analyze-uri! (fn [& _])
                    built-in/analyze-uri! (fn [& _] {})
                    custom/analyze-uri! (fn [& _] {})
                    diagnostics/publish-diagnostics! (fn [& _] (swap! published* inc))]
        (f.file-management/analyze-changes {:uri uri :text "[1]" :version 0} (h/components)))
      (is (zero? @published*))
      (is (= -1 (get-in @db* [:documents uri :analyzed-version])))
      (is (= "[2]" (get-in @db* [:documents uri :text]))))))

(deftest opening-resets-analysis-version
  (let [uri h/default-uri]
    (swap! (h/db*) assoc-in [:documents uri] {:analyzed-version 42 :closed? true})
    (f.file-management/load-document! uri "{:a 1}" (h/db*))
    (is (= -1 (get-in (h/db) [:documents uri :analyzed-version])))
    (is (false? (get-in (h/db) [:documents uri :closed?])))
    (h/load-code-and-locs "{:a 1}" uri)
    (is (= 0 (get-in (h/db) [:documents uri :v])))
    (is (= 0 (get-in (h/db) [:documents uri :analyzed-version])))))

(deftest update-text
  ;; line endings the same as those of the underlying system's.
  (is (= (h/lf->sys "(comment\n   )") (#'f.file-management/replace-text "(comment)" "\n   " 0 8 0 8)))
  (is (= (h/lf->sys "(comment\n   )") (#'f.file-management/replace-text "(comment)" "\r\n   " 0 8 0 8)))

  ;; line endings should be those of the original text's.
  (is (= "some \nboring\n text" (#'f.file-management/replace-text "some \ncool\n text" "boring" 1 0 1 4)))
  (is (= "some \r\nboring\r\n text" (#'f.file-management/replace-text "some \r\ncool\r\n text" "boring" 1 0 1 4)))

  (is (= "(+ 1 2)" (#'f.file-management/replace-text "(+ 1 1)" "2" 0 5 0 6)))
  (is (= "(+ 1)" (#'f.file-management/replace-text "(+ 1 1)" "" 0 4 0 6)))

  (is (= "\n\n (+ 1 2)\n" (#'f.file-management/replace-text "\n\n (let [a 1\n   b 2]\n   (+ 1 2))\n" "(+ 1 2)" 2 1 4 11)))
  (is (= "\r\n\r\n (+ 1 2)\r\n" (#'f.file-management/replace-text "\r\n\r\n (let [a 1\r\n   b 2]\r\n   (+ 1 2))\r\n" "(+ 1 2)" 2 1 4 11)))

  (is (= "\n\n (let [a 1\n   b 2]\n   (+ 1 2))\n" (#'f.file-management/replace-text "\n\n (+ 1 2)\n" "(let [a 1\n   b 2]\n   (+ 1 2))" 2 1 2 8)))
  (is (= "\r\n\r\n (let [a 1\r\n   b 2]\r\n   (+ 1 2))\r\n" (#'f.file-management/replace-text "\r\n\r\n (+ 1 2)\r\n" "(let [a 1\r\n   b 2]\r\n   (+ 1 2))" 2 1 2 8)))

  (is (= "(+ 1 1)\n\n" (#'f.file-management/replace-text "(+ 1 1)\n" "\n" 1 0 1 0)))
  (is (= "(+ 1 1)\n\n" (#'f.file-management/replace-text "(+ 1 1)\n" "\r\n" 1 0 1 0)))
  (is (= "(+ 1 1)\r\n\r\n" (#'f.file-management/replace-text "(+ 1 1)\r\n" "\n" 1 0 1 0)))
  (is (= "(+ 1 1)\r\n\r\n" (#'f.file-management/replace-text "(+ 1 1)\r\n" "\r\n" 1 0 1 0))))

(deftest did-close
  (swap! (h/db*) medley/deep-merge {:settings {:source-paths #{(h/file-path "/user/project/src/clj")}}
                                    :project-root-uri (h/file-uri "file:///user/project")})
  (h/load-code-and-locs "(ns foo) a b c" (h/file-uri "file:///user/project/src/clj/foo.clj"))
  (h/load-code-and-locs "(ns bar) d e f" (h/file-uri "file:///user/project/src/clj/bar.clj"))
  (h/load-code-and-locs "(ns some-jar)" (h/file-uri "file:///some/path/to/jar.jar:/some/file.clj"))
  (testing "when file exists on disk"
    (let [mock-diagnostics-chan (h/make-diagnostics-channel)
          uri (h/file-uri "file:///user/project/src/clj/foo.clj")]
      (with-redefs [shared/file-exists? (constantly true)]
        (f.file-management/did-close uri (assoc (h/components)
                                                :diagnostics-chan mock-diagnostics-chan)))
      (is (get-in (h/db) [:analysis uri]))
      (is (get-in (h/db) [:diagnostics :clj-kondo uri]))
      (is (seq (get-in (h/db) [:dep-graph 'foo :uris])))
      (is (get-in (h/db) [:documents uri]))
      (is (true? (get-in (h/db) [:documents uri :closed?])))
      (h/assert-no-take mock-diagnostics-chan 500)))
  (testing "when local file not exists on disk"
    (let [mock-diagnostics-chan (h/make-diagnostics-channel)
          uri (h/file-uri "file:///user/project/src/clj/bar.clj")]
      (with-redefs [shared/file-exists? (constantly false)]
        (f.file-management/did-close uri (assoc (h/components)
                                                :diagnostics-chan mock-diagnostics-chan)))
      (is (nil? (get-in (h/db) [:analysis uri])))
      (is (nil? (get-in (h/db) [:diagnostics :clj-kondo uri])))
      (is (not (seq (get-in (h/db) [:dep-graph 'bar :uris]))))
      (is (nil? (get-in (h/db) [:documents uri])))
      (is (= {:uri uri
              :diagnostics []}
             (h/take-or-timeout mock-diagnostics-chan 500)))))
  (testing "when file is external we do not remove analysis but publish empty diagnostics"
    (let [mock-diagnostics-chan (h/make-diagnostics-channel)
          uri (h/file-uri "file:///some/path/to/jar.jar:/some/file.clj")]
      (with-redefs [shared/file-exists? (constantly false)]
        (f.file-management/did-close uri (assoc (h/components)
                                                :diagnostics-chan mock-diagnostics-chan)))
      (is (get-in (h/db) [:analysis uri]))
      (is (get-in (h/db) [:diagnostics :clj-kondo uri]))
      (is (seq (get-in (h/db) [:dep-graph 'some-jar :uris])))
      (is (get-in (h/db) [:documents uri]))
      (is (true? (get-in (h/db) [:documents uri :closed?])))
      (is (= {:uri uri
              :diagnostics []}
             (h/take-or-timeout mock-diagnostics-chan 500))))))

(deftest did-open
  (testing "on an empty file"
    (let [mock-edits-chan (async/chan 1)
          mock-diagnostics-chan (h/make-diagnostics-channel)
          uri (h/file-uri "file:///user/project/src/aaa/bbb.clj")]
      (swap! (h/db*) shared/deep-merge {:settings {:auto-add-ns-to-new-files? true
                                                   :source-paths #{(h/file-path "/user/project/src")}}
                                        :project-root-uri (h/file-uri "file:///user/project")})
      (h/load-code-and-locs "" uri (assoc (h/components)
                                          :edits-chan mock-edits-chan
                                          :diagnostics-chan mock-diagnostics-chan))
      (is (get-in (h/db) [:analysis uri]))
      (is (get-in (h/db) [:diagnostics :clj-kondo uri]))
      ;; The ns won't be in the dep graph until after the edit adding it is applied.
      (is (not (contains? (get (h/db) :dep-graph) 'aaa.bbb)))
      (is (get-in (h/db) [:documents uri]))
      (testing "should publish empty diagnostics"
        (is (= {:uri uri, :diagnostics []}
               (h/take-or-timeout mock-diagnostics-chan 500))))
      (testing "should add ns"
        (is (= {:changes
                {uri
                 [{:range
                   {:start {:line 0, :character 0},
                    :end {:line 999998, :character 999998}},
                   :new-text "(ns aaa.bbb)"}]}}
               (h/take-or-timeout mock-edits-chan 500)))))))

(deftest did-change
  (let [mock-changes-chan (async/chan 1)
        original-text (h/code "(ns aaa)"
                              "(def foo 1)")
        edited-text (h/code "(ns aaa)"
                            "(def bar 1)")]
    (h/load-code-and-locs original-text)
    (f.file-management/did-change h/default-uri
                                  [{:text "bar"
                                    :range {:start {:line 1 :character 5}
                                            :end {:line 1 :character 8}}}]
                                  2
                                  (assoc (h/components)
                                         :current-changes-chan mock-changes-chan))
    (is (= 2 (get-in (h/db) [:documents h/default-uri :v])))
    (is (= edited-text (get-in (h/db) [:documents h/default-uri :text])))
    (is (= {:uri h/default-uri, :text edited-text, :version 2
            :open-session (get-in (h/db) [:documents h/default-uri :open-session])}
           (h/take-or-timeout mock-changes-chan 500)))))

(deftest did-change-watched-files
  (swap! (h/db*) medley/deep-merge {:project-root-uri (h/file-uri "file:///")})
  (testing "created file"
    (let [mock-watched-files-chan (async/chan 1)]
      (f.file-management/did-change-watched-files
        [{:type :created
          :uri h/default-uri}]
        (assoc (h/components)
               :watched-files-chan mock-watched-files-chan))
      (is (= {:uri h/default-uri :type :created :document-state {}} (h/take-or-timeout mock-watched-files-chan 1000)))))
  (testing "changed file"
    (let [mock-watched-files-chan (async/chan 1)]
      (f.file-management/did-change-watched-files
        [{:type :changed
          :uri h/default-uri}]
        (assoc (h/components)
               :watched-files-chan mock-watched-files-chan))
      (is (= {:uri h/default-uri :type :changed :document-state {}} (h/take-or-timeout mock-watched-files-chan 1000)))))
  (testing "deleted file is queued with other events"
    (let [mock-watched-files-chan (async/chan 1)]
      (f.file-management/did-change-watched-files
        [{:type :deleted :uri h/default-uri}]
        (assoc (h/components) :watched-files-chan mock-watched-files-chan))
      (is (= {:uri h/default-uri :type :deleted :document-state {}}
             (h/take-or-timeout mock-watched-files-chan 500)))))
  (testing "watched files ignored by source-path-ignore-regex"
    (swap! (h/db*) medley/deep-merge {:settings {:source-paths #{(h/file-path "/target")
                                                                 (h/file-path "/src")}}
                                      :project-root-uri (h/file-uri "file:///project")})
    (let [mock-watched-files-chan (async/chan 2)]
      (f.file-management/did-change-watched-files
        [{:type :changed
          :uri (h/file-uri "file:///project/target/a.clj")}
         {:type :changed
          :uri (h/file-uri "file:///project/src/a.clj")}]
        (assoc (h/components)
               :watched-files-chan mock-watched-files-chan))
      (is (= {:uri (h/file-uri "file:///project/src/a.clj") :type :changed :document-state {}}
             (h/take-or-timeout mock-watched-files-chan 1000)))
      (h/assert-no-take mock-watched-files-chan 500))))

(deftest analyze-watched-files
  (let [project-dir (fs/canonicalize (fs/create-temp-dir {:prefix "clojure-lsp-watched-files"}))
        src-dir (fs/create-dirs (fs/path project-dir "src"))
        _ (fs/create-dirs (fs/path project-dir ".clj-kondo"))
        a-file (fs/file src-dir "a.clj")
        b-file (fs/file src-dir "b.clj")]
    (try
      (swap! (h/db*) medley/deep-merge {:project-root-uri (shared/filename->uri (str project-dir) (h/db))
                                        :settings {:source-paths #{(str src-dir)}}})
      (let [a-uri (shared/filename->uri (str a-file) (h/db))
            b-uri (shared/filename->uri (str b-file) (h/db))
            a-unused-public-vars #(count (get-in (h/db) [:diagnostics :built-in a-uri]))]
        (spit a-file (h/code "(ns a)"
                             "(defn foo [] 1)"))
        (spit b-file (h/code "(ns b (:require [a]))"
                             "(a/foo)"))
        (h/load-code (slurp a-file) a-uri)
        (is (= 1 (a-unused-public-vars)) "a/foo is unused while b is unknown")
        (testing "returns only after the files depending on a created file are re-analyzed"
          (f.file-management/analyze-watched-files! [{:uri b-uri :type :created}] (h/components))
          (is (= 0 (a-unused-public-vars)) "a/foo is used by b"))
        (testing "a file deleted and re-created within one batch keeps its open document state"
          (h/load-code (slurp b-file) b-uri)
          (f.file-management/analyze-watched-files! [{:uri b-uri :type :deleted}
                                                     {:uri b-uri :type :created}]
                                                    (h/components))
          (is (= 0 (get-in (h/db) [:documents b-uri :v])))
          (is (= 0 (a-unused-public-vars)) "a/foo is still used by b"))
        (testing "returns only after the files depending on a deleted file are re-analyzed"
          (fs/delete b-file)
          (f.file-management/analyze-watched-files! [{:uri b-uri :type :deleted}] (h/components))
          (is (nil? (get-in (h/db) [:analysis b-uri])))
          (is (= 1 (a-unused-public-vars)) "a/foo is unused again")))
      (finally
        (fs/delete-tree project-dir)))))

(deftest var-dependency-reference-uris
  (swap! (h/db*) medley/deep-merge {:settings {:source-paths #{(h/file-path "/src")}}
                                    :project-root-uri (h/file-uri "file:///")})
  (h/load-code-and-locs (h/code "(ns a)"
                                "(def a)"
                                "(def b)") (h/file-uri "file:///src/a.clj"))
  (h/load-code-and-locs (h/code "(ns b (:require [a]))"
                                "(def x)"
                                "a/a"
                                "a/a") (h/file-uri "file:///src/b.clj"))
  (let [db-before (h/db)]
    (are [expected new-code]
         (do
           (h/load-code-and-locs new-code (h/file-uri "file:///src/b.clj"))
           (let [db-after (h/db)]
             (is (= expected
                    (f.file-management/reference-uris (h/file-uri "file:///src/b.clj") db-before db-after)))))
      ;; increasing
      #{} (h/code "(ns b (:require [a]))"
                  "(def x)"
                  "a/a"
                  "a/a"
                  "a/a")
      ;; decreasing
      #{} (h/code "(ns b (:require [a]))"
                  "(def x)"
                  "a/a")
      ;; removing
      #{(h/file-uri "file:///src/a.clj")} (h/code "(ns b (:require [a]))"
                                                  "(def x)")
      ;; adding
      #{(h/file-uri "file:///src/a.clj")} (h/code "(ns b (:require [a]))"
                                                  "(def x)"
                                                  "a/a"
                                                  "a/a"
                                                  "a/b")
      ;; same
      #{} (h/code "(ns b (:require [a]))"
                  "(def x)"
                  "a/a"
                  "a/a")
      ;; external ns
      #{} (h/code "(ns b (:require [a]))"
                  "(def x)"
                  "a/a"
                  "a/a"
                  "inc")
      ;; same ns
      #{} (h/code "(ns b (:require [a]))"
                  "(def x)"
                  "a/a"
                  "a/a"
                  "x"))

    (testing "When a file is removed"
      (h/delete-file (h/file-uri "file:///src/b.clj"))

      (is (= #{(h/file-uri "file:///src/a.clj")}
             (f.file-management/reference-uris (h/file-uri "file:///src/b.clj") (h/db) db-before))))))

(deftest kw-dependency-reference-uris
  (swap! (h/db*) medley/deep-merge {:settings {:source-paths #{(h/file-path "/src")}}
                                    :project-root-uri (h/file-uri "file:///")})
  (h/load-code-and-locs (h/code "(ns aaa (:require [re-frame.core :as r]))"
                                "(r/reg-event-db :aaa/command identity)"
                                "(r/reg-event-db ::event identity)")
                        (h/file-uri "file:///src/aaa.clj"))
  (h/load-code-and-locs (h/code "(ns bbb (:require [re-frame.core :as r]))"
                                "(r/reg-event-db :bbb/command identity)"
                                ":aaa/command"
                                ":aaa/command")
                        (h/file-uri "file:///src/bbb.clj"))
  (let [db-before (h/db)]
    (are [expected new-code]
         (do
           (h/load-code-and-locs new-code (h/file-uri "file:///src/bbb.clj"))
           (let [db-after (h/db)]
             (is (= expected
                    (f.file-management/reference-uris (h/file-uri "file:///src/bbb.clj") db-before db-after)))))
      ;; increasing
      #{} (h/code "(ns bbb (:require [re-frame.core :as r]))"
                  "(r/reg-event-db :bbb/command identity)"
                  ":aaa/command"
                  ":aaa/command"
                  ":aaa/command")
      ;; decreasing
      #{} (h/code "(ns bbb (:require [re-frame.core :as r]))"
                  "(r/reg-event-db :bbb/command identity)"
                  ":aaa/command")
      ;; removing
      #{(h/file-uri "file:///src/aaa.clj")} (h/code "(ns bbb (:require [re-frame.core :as r]))"
                                                    "(r/reg-event-db :bbb/command identity)")
      ;; adding
      #{(h/file-uri "file:///src/aaa.clj")} (h/code "(ns bbb (:require [re-frame.core :as r]))"
                                                    "(r/reg-event-db :bbb/command identity)"
                                                    ":aaa/command"
                                                    ":aaa/command"
                                                    ":aaa/event")
      ;; same
      #{} (h/code "(ns bbb (:require [re-frame.core :as r]))"
                  "(r/reg-event-db :bbb/command identity)"
                  ":aaa/command"
                  ":aaa/command")
      ;; unregistered kw
      #{} (h/code "(ns bbb (:require [re-frame.core :as r]))"
                  "(r/reg-event-db :bbb/command identity)"
                  ":aaa/command"
                  ":aaa/command"
                  ":unregistered")
      ;; same ns
      #{} (h/code "(ns bbb (:require [re-frame.core :as r]))"
                  "(r/reg-event-db :bbb/command identity)"
                  ":aaa/command"
                  ":aaa/command"
                  ":bbb/command"))))

(deftest var-dependent-reference-uris
  (swap! (h/db*) medley/deep-merge {:settings {:source-paths #{(h/file-path "/src")}}
                                    :project-root-uri (h/file-uri "file:///")})
  (h/load-code-and-locs (h/code "(ns a)"
                                "(def a)"
                                "(def b)") (h/file-uri "file:///src/a.clj"))
  (h/load-code-and-locs (h/code "(ns b (:require [a]))"
                                "a/a"
                                "a/c") (h/file-uri "file:///src/b.clj"))
  (let [db-before (h/db)]
    (are [expected new-code]
         (do
           (h/load-code-and-locs new-code (h/file-uri "file:///src/a.clj"))
           (let [db-after (h/db)]
             (is (= expected
                    (f.file-management/reference-uris (h/file-uri "file:///src/a.clj") db-before db-after)))))
      ;; remove existing
      #{(h/file-uri "file:///src/b.clj")} (h/code "(ns a)"
                                                  "(def b)")
      ;; create missing
      #{(h/file-uri "file:///src/b.clj")} (h/code "(ns a)"
                                                  "(def a)"
                                                  "(def b)"
                                                  "(def c)")
      ;; remove unused
      #{} (h/code "(ns a)"
                  "(def a)")
      ;; add unused
      #{} (h/code "(ns a)"
                  "(def a)"
                  "(def b)"
                  "(def d)"))))

(deftest will-rename-files
  (testing "when namespace matches old file"
    (swap! (h/db*) shared/deep-merge {:settings {:source-paths #{(h/file-path "/user/project/src")}}
                                      :project-root-uri (h/file-uri "file:///user/project")
                                      :client-capabilities {:workspace {:workspace-edit {:document-changes true}}}})

    (let [old-uri (h/file-uri "file:///user/project/src/my/ns.clj")
          new-uri (h/file-uri "file:///user/project/src/my/new/ns.clj")]
      (h/load-code (h/code "(ns my.ns)") old-uri)
      (let [db (h/db)]
        (is (= {:document-changes
                [{:text-document
                  {:version 0, :uri (h/file-uri "file:///user/project/src/my/ns.clj")},
                  :edits
                  [{:range
                    {:start {:line 0, :character 4}, :end {:line 0, :character 9}},
                    :new-text "my.new.ns"}]}]}
               (f.file-management/will-rename-files
                 [{:old-uri old-uri
                   :new-uri new-uri}]
                 db))))))
  (testing "when namespace matches new file"
    ;; This happens when namespace was already changed by textDocument/rename
    (swap! (h/db*) shared/deep-merge {:settings {:source-paths #{(h/file-path "/user/project/src")}}
                                      :project-root-uri (h/file-uri "file:///user/project")
                                      :client-capabilities {:workspace {:workspace-edit {:document-changes true}}}})

    (let [old-uri (h/file-uri "file:///user/project/src/my/ns.clj")
          new-uri (h/file-uri "file:///user/project/src/my/new/ns.clj")]
      (h/load-code (h/code "(ns my.new.ns)") old-uri)
      (let [db (h/db)]
        (is (= {:document-changes []}
               (f.file-management/will-rename-files
                 [{:old-uri old-uri
                   :new-uri new-uri}]
                 db))))))
  (testing "when other files use the namespace vars fully qualified"
    (h/reset-components!)
    (swap! (h/db*) shared/deep-merge {:settings {:source-paths #{(h/file-path "/user/project/src")}}
                                      :project-root-uri (h/file-uri "file:///user/project")
                                      :client-capabilities {:workspace {:workspace-edit {:document-changes true}}}})

    (let [old-uri (h/file-uri "file:///user/project/src/my/ns.clj")
          new-uri (h/file-uri "file:///user/project/src/my/new/ns.clj")
          user-uri (h/file-uri "file:///user/project/src/my/user.clj")]
      (h/load-code (h/code "(ns my.ns)"
                           "(defn area [w h] (* w h))") old-uri)
      (h/load-code (h/code "(ns my.user"
                           "  (:require [my.ns :as s]))"
                           "(s/area 1 2)"
                           "(my.ns/area 5 6)") user-uri)
      (is (= {:document-changes
              [{:text-document {:version 0, :uri old-uri},
                :edits
                [{:range {:start {:line 0, :character 4}, :end {:line 0, :character 9}},
                  :new-text "my.new.ns"}]}
               {:text-document {:version 0, :uri user-uri},
                :edits
                [{:range {:start {:line 1, :character 13}, :end {:line 1, :character 18}},
                  :new-text "my.new.ns"}
                 {:range {:start {:line 3, :character 1}, :end {:line 3, :character 6}},
                  :new-text "my.new.ns"}]}]}
             (f.file-management/will-rename-files
               [{:old-uri old-uri
                 :new-uri new-uri}]
               (h/db))))))
  (testing "when other cljc files use the namespace vars fully qualified"
    ;; cljc files are analyzed for each lang, but each usage must be edited once,
    ;; as clients reject or double apply overlapping edits
    (h/reset-components!)
    (swap! (h/db*) shared/deep-merge {:settings {:source-paths #{(h/file-path "/user/project/src")}}
                                      :project-root-uri (h/file-uri "file:///user/project")
                                      :client-capabilities {:workspace {:workspace-edit {:document-changes true}}}})

    (let [old-uri (h/file-uri "file:///user/project/src/my/ns.cljc")
          new-uri (h/file-uri "file:///user/project/src/my/new/ns.cljc")
          user-uri (h/file-uri "file:///user/project/src/my/user.cljc")]
      (h/load-code (h/code "(ns my.ns)"
                           "(defn area [w h] (* w h))") old-uri)
      (h/load-code (h/code "(ns my.user"
                           "  (:require [my.ns :as s]))"
                           "(s/area 1 2)"
                           "(my.ns/area 5 6)") user-uri)
      (is (= {:document-changes
              [{:text-document {:version 0, :uri old-uri},
                :edits
                [{:range {:start {:line 0, :character 4}, :end {:line 0, :character 9}},
                  :new-text "my.new.ns"}]}
               {:text-document {:version 0, :uri user-uri},
                :edits
                [{:range {:start {:line 1, :character 13}, :end {:line 1, :character 18}},
                  :new-text "my.new.ns"}
                 {:range {:start {:line 3, :character 1}, :end {:line 3, :character 6}},
                  :new-text "my.new.ns"}]}]}
             (f.file-management/will-rename-files
               [{:old-uri old-uri
                 :new-uri new-uri}]
               (h/db)))))))

(deftest analyze-reference-uris
  (let [project-dir (fs/canonicalize (fs/create-temp-dir {:prefix "clojure-lsp-reference-uris"}))
        src-dir (fs/create-dirs (fs/path project-dir "src"))
        _ (fs/create-dirs (fs/path project-dir ".clj-kondo"))
        a-file (fs/file src-dir "a.clj")
        b-file (fs/file src-dir "b.clj")
        c-file (fs/file src-dir "c.clj")]
    (try
      (swap! (h/db*) medley/deep-merge {:project-root-uri (shared/filename->uri (str project-dir) (h/db))
                                        :settings {:source-paths #{(str src-dir)}}})
      (let [a-uri (shared/filename->uri (str a-file) (h/db))
            b-uri (shared/filename->uri (str b-file) (h/db))
            c-uri (shared/filename->uri (str c-file) (h/db))
            var-names (fn [uri]
                        (into #{} (map :name) (get-in (h/db) [:analysis uri :var-definitions])))
            kondo-finding-types (fn [uri]
                                  (into #{} (map :type) (get-in (h/db) [:diagnostics :clj-kondo uri])))]
        (spit a-file (h/code "(ns a)"
                             "(defn foo [] 1)"))
        (spit b-file (h/code "(ns b)"
                             "(defn baz [] 1)"))
        ;; a is open with unsaved changes, b is not open
        (h/load-code (h/code "(ns a)"
                             "(defn foo [] 1)"
                             "(defn bar [x y] (+ x y))")
                     a-uri)
        (f.file-management/analyze-reference-uris! #{a-uri b-uri} (h/db*))
        (testing "open documents are linted from their unsaved text"
          (is (= '#{foo bar} (var-names a-uri))))
        (testing "files that are not open are linted from disk"
          (is (= '#{baz} (var-names b-uri))))
        (testing "clj-kondo cache keeps the unsaved definitions of open documents"
          (h/load-code (h/code "(ns c (:require [a]))"
                               "(a/bar 1 2 3)")
                       c-uri)
          (is (= #{:invalid-arity} (kondo-finding-types c-uri))
              "a/bar is known with its unsaved arity, so it's not reported as unresolved")))
      (finally
        (fs/delete-tree project-dir)))))

(deftest reference-analysis-reuses-only-valid-snapshots
  (doseq [[label update-db expected-runs]
          [["bookkeeping" #(assoc % :progress :updated) 1]
           ["project usages" #(assoc-in % [:analysis "other" :keyword-usages] [{:name 'used}]) 2]
           ["dependency graph" #(assoc-in % [:dep-graph 'other :internal?] true) 2]
           ["ignore comments" #(assoc-in % [:documents "other" :text] "#_:clojure-lsp/ignore (def x 1)") 2]
           ["settings" #(assoc-in % [:settings :linters :clojure-lsp/unused-public-var :level] :off) 2]]]
    (testing label
      (h/reset-components!)
      (let [uri h/default-uri
            db* (h/db*)
            kondo-runs* (atom 0)
            linter-runs* (atom 0)]
        (swap! db* assoc :analysis {uri {}})
        (with-redefs [shared/file-exists? (constantly true)
                      kondo/run-kondo-on-reference-uris!
                      (fn [uris _]
                        (swap! kondo-runs* inc)
                        {:analysis (zipmap uris (repeat {})) :findings (zipmap uris (repeat []))})
                      built-in/analyze-uri!
                      (fn [uri state]
                        (when (= 1 (swap! linter-runs* inc))
                          (swap! db* update-db))
                        {uri [{:message (if (:progress state) "updated" "snapshot")}]})]
          (f.file-management/analyze-reference-uris! [uri] db*))
        (is (= expected-runs @kondo-runs*))
        (is (= expected-runs @linter-runs*))
        (is (seq (get-in @db* [:diagnostics :built-in uri])))
        (when (= label "bookkeeping")
          (is (= :updated (:progress @db*))))))))

(deftest reference-analysis-preserves-new-editor-sessions
  (doseq [change [:edit :reopen :delete]]
    (testing (name change)
      (h/reset-components!)
      (let [uri h/default-uri
            db* (h/db*)
            runs* (atom [])
            session (get-in (f.file-management/load-document! uri "old" db*)
                            [:documents uri :open-session])]
        (swap! db* assoc :analysis {uri {}})
        (with-redefs [kondo/run-kondo-on-text!
                      (fn [text uri _]
                        (swap! runs* conj text)
                        (when (= 1 (count @runs*))
                          (case change
                            :edit (swap! db* update-in [:documents uri] assoc :text "new" :v 1)
                            :reopen (f.file-management/load-document! uri "new" db*)
                            :delete (swap! db* kondo/db-without-uris [uri])))
                        {:analysis {uri {:text text}} :findings {uri []}})
                      built-in/analyze-uri! (fn [uri state] {uri [{:message (get-in state [:analysis uri :text])}]})]
          (f.file-management/analyze-reference-uris! [uri] db*))
        (if (= :delete change)
          (do (is (= ["old"] @runs*))
              (is (nil? (get-in @db* [:analysis uri])))
              (is (nil? (get-in @db* [:diagnostics :built-in uri]))))
          (do (is (= ["old" "new"] @runs*))
              (is (= "new" (get-in @db* [:analysis uri :text])))
              (is (= [{:message "new"}] (get-in @db* [:diagnostics :built-in uri])))
              (when (= :reopen change)
                (is (not= session (get-in @db* [:documents uri :open-session])))
                (is (zero? (get-in @db* [:documents uri :v]))))))))))

(deftest bulk-watched-files-share-one-reference-analysis
  (let [sources (mapv #(h/file-uri (str "file:///src/source" % ".clj")) (range 200))
        target (h/file-uri "file:///src/target.clj")
        db* (h/db*)
        reference-batches* (atom [])
        active* (atom 0)
        max-active* (atom 0)]
    (swap! db* assoc :analysis (zipmap (conj sources target) (repeat {})))
    (with-redefs [shared/file-exists? (constantly true)
                  f.file-management/reference-uris (fn [_ _ _] #{target})
                  kondo/run-kondo-on-reference-uris!
                  (fn [uris _]
                    (let [active (swap! active* inc)]
                      (swap! max-active* max active)
                      (when (= [target] (vec uris))
                        (swap! reference-batches* conj (set uris)))
                      (swap! active* dec)
                      {:analysis (zipmap uris (repeat {})) :findings (zipmap uris (repeat []))}))
                  built-in/analyze-uri! (fn [uri _] {uri []})
                  built-in/analyze-uris! (fn [uris _] (zipmap uris (repeat [])))
                  diagnostics/publish-all-diagnostics! (fn [& _])
                  producer/refresh-code-lens (fn [& _])]
      (f.file-management/analyze-watched-files! (mapv #(hash-map :uri % :type :changed) sources)
                                                (h/components)))
    ;; Completion is synchronous, so there are no outstanding jobs to await.
    (is (= [#{target}] @reference-batches*))
    (is (= 1 @max-active*))
    (is (zero? @active*))))

(deftest watched-files-use-the-final-event
  (let [deleted (h/file-uri "file:///src/deleted.clj")
        recreated (h/file-uri "file:///src/recreated.clj")
        db* (h/db*)
        batches* (atom [])]
    (swap! db* assoc :analysis {deleted {} recreated {}}
           :settings {:notify-references-on-file-change false})
    (with-redefs [shared/file-exists? (constantly true)
                  kondo/run-kondo-on-reference-uris!
                  (fn [uris _]
                    (swap! batches* conj (set uris))
                    {:analysis (zipmap uris (repeat {:fresh true})) :findings (zipmap uris (repeat []))})
                  built-in/analyze-uri! (fn [uri _] {uri []})
                  diagnostics/publish-all-diagnostics! (fn [& _])
                  diagnostics/publish-empty-diagnostics! (fn [& _])]
      (f.file-management/analyze-watched-files!
        [{:uri deleted :type :created} {:uri recreated :type :deleted}
         {:uri deleted :type :deleted} {:uri recreated :type :created}
         {:uri recreated :type :changed}]
        (h/components)))
    (is (= [#{recreated}] @batches*))
    (is (nil? (get-in @db* [:analysis deleted])))
    (is (= {:fresh true} (get-in @db* [:analysis recreated])))))

(deftest deleted-namespaces-still-notify-surviving-dependents
  (let [deleted (h/file-uri "file:///src/a.clj")
        dependent (h/file-uri "file:///src/b.clj")
        db* (h/db*)
        batches* (atom [])]
    (h/load-code-and-locs "(ns a) (def foo 1)" deleted)
    (h/load-code-and-locs "(ns b (:require [a :as a])) (a/foo)" dependent)
    (with-redefs [kondo/run-kondo-on-text!
                  (fn [_ uri _]
                    (swap! batches* conj uri)
                    {:analysis {uri (get-in @db* [:analysis uri])} :findings {uri []}})
                  built-in/analyze-uri! (fn [uri _] {uri []})
                  diagnostics/publish-all-diagnostics! (fn [& _])
                  diagnostics/publish-empty-diagnostics! (fn [& _])
                  producer/refresh-code-lens (fn [& _])]
      (f.file-management/analyze-watched-files! [{:uri deleted :type :deleted}] (h/components)))
    (is (= [dependent] @batches*))
    (is (nil? (get-in @db* [:analysis deleted])))))

(deftest reference-gate-recovers-after-failure
  (let [uri h/default-uri
        db* (h/db*)]
    (swap! db* assoc :analysis {uri {}})
    (with-redefs [shared/file-exists? (constantly true)
                  kondo/run-kondo-on-reference-uris! (fn [& _] (throw (ex-info "failed analysis" {})))]
      (is (thrown-with-msg? clojure.lang.ExceptionInfo #"failed analysis"
                            (f.file-management/analyze-reference-uris! [uri] db*))))
    (with-redefs [shared/file-exists? (constantly true)
                  kondo/run-kondo-on-reference-uris! (fn [& _] {:analysis {uri {}} :findings {uri []}})
                  built-in/analyze-uri! (fn [uri _] {uri []})]
      (is (= [uri] (:uris (f.file-management/analyze-reference-uris! [uri] db*)))))))

(deftest reference-analysis-rejects-stale-unused-diagnostics
  (let [source (h/file-uri "file:///src/a.clj")
        consumer (h/file-uri "file:///src/b.clj")
        db* (h/db*)
        original-analyze built-in/analyze-uri!
        runs* (atom 0)]
    (h/load-code-and-locs "(ns a) (def foo 1)" source)
    (h/load-code-and-locs "(ns b (:require [a]))" consumer)
    (let [new-usage (kondo/run-kondo-on-text! "(ns b (:require [a])) a/foo" consumer (atom @db*))]
      (with-redefs [built-in/analyze-uri!
                    (fn [uri state]
                      (let [diags (original-analyze uri state)]
                        (when (= 1 (swap! runs* inc))
                          (is (some #(= "clojure-lsp/unused-public-var" (:code %)) (get diags source)))
                          (swap! db* kondo/db-with-results new-usage))
                        diags))]
        (f.file-management/analyze-reference-uris! [source] db*)))
    (is (= 2 @runs*))
    (is (empty? (get-in @db* [:diagnostics :built-in source])))))

(deftest reference-analysis-is-serialized-before-starting-work
  (let [uri h/default-uri
        db* (h/db*)
        entered (promise)
        release (promise)
        attempted (promise)
        calls* (atom 0)
        errors* (atom [])
        run #(try (f.file-management/analyze-reference-uris! [uri] db*)
                  (catch Throwable e (swap! errors* conj e)))
        first-worker (Thread. ^Runnable run)
        second-worker (Thread. ^Runnable #(do (deliver attempted true) (run)))]
    (swap! db* assoc :analysis {uri {}})
    (with-redefs [shared/file-exists? (constantly true)
                  kondo/run-kondo-on-reference-uris!
                  (fn [& _]
                    (when (= 1 (swap! calls* inc))
                      (deliver entered true)
                      (deref release 5000 nil))
                    {:analysis {uri {}} :findings {uri []}})
                  built-in/analyze-uri! (fn [uri _] {uri []})]
      (try
        (.start first-worker)
        (is (= true (deref entered 5000 :timeout)))
        (.start second-worker)
        (is (= true (deref attempted 5000 :timeout)))
        (let [deadline (+ (System/nanoTime) 2000000000)]
          (loop []
            (when (and (.isAlive second-worker)
                       (not= Thread$State/BLOCKED (.getState second-worker))
                       (< (System/nanoTime) deadline))
              (Thread/sleep 1)
              (recur))))
        (is (= Thread$State/BLOCKED (.getState second-worker)))
        (is (= 1 @calls*))
        (finally
          (deliver release true)
          (.join first-worker 5000)
          (.join second-worker 5000))))
    (is (empty? @errors*))
    (is (= 2 @calls*))
    (is (not (.isAlive first-worker)))
    (is (not (.isAlive second-worker)))))

(deftest watched-file-diagnostics-follow-disk-changes
  (let [root (fs/create-temp-dir {:prefix "clojure-lsp-watched-"})
        db* (h/db*)
        a-path (str (fs/path root "a.clj"))
        b-path (str (fs/path root "b.clj"))
        a (shared/filename->uri a-path @db*)
        b (shared/filename->uri b-path @db*)
        analyze #(f.file-management/analyze-watched-files! % (h/components))]
    (try
      (swap! db* assoc :project-root-uri (shared/filename->uri (str root) @db*)
             :settings {:source-paths #{(str root)}})
      (spit a-path "(ns a) (def foo 1)")
      (spit b-path "(ns b (:require [a])) a/foo")
      (analyze [{:uri a :type :created} {:uri b :type :created}])
      (is (empty? (get-in @db* [:diagnostics :built-in a])))
      (is (not-any? #(= :unresolved-var (:type %)) (get-in @db* [:diagnostics :clj-kondo b])))
      (spit b-path "(ns b (:require [a]))")
      (analyze [{:uri b :type :changed}])
      (is (some #(= "clojure-lsp/unused-public-var" (:code %))
                (get-in @db* [:diagnostics :built-in a])))
      (spit b-path "(ns b (:require [a])) a/foo")
      (analyze [{:uri b :type :changed}])
      (is (empty? (get-in @db* [:diagnostics :built-in a])))
      (spit a-path "(ns a) (def bar 2)")
      (analyze [{:uri a :type :changed}])
      (is (some #(= :unresolved-var (:type %)) (get-in @db* [:diagnostics :clj-kondo b])))
      (spit a-path "(ns a) (def foo 2)")
      (analyze [{:uri a :type :deleted} {:uri a :type :created}])
      (is (not-any? #(= :unresolved-var (:type %)) (get-in @db* [:diagnostics :clj-kondo b])))
      (let [c-path (str (fs/path root "c.clj"))
            c (shared/filename->uri c-path @db*)]
        (fs/move a-path c-path)
        (spit c-path "(ns c) (def foo 2)")
        (spit b-path "(ns b (:require [c])) c/foo")
        (analyze [{:uri a :type :deleted} {:uri c :type :created} {:uri b :type :changed}])
        (is (nil? (get-in @db* [:analysis a])))
        (is (= 'c (get-in @db* [:analysis c :namespace-definitions 0 :name])))
        (is (empty? (get-in @db* [:diagnostics :built-in c])))
        (is (not-any? #(contains? #{:unresolved-var :unresolved-namespace} (:type %))
                      (get-in @db* [:diagnostics :clj-kondo b])))
        (fs/delete c-path)
        (analyze [{:uri c :type :deleted}])
        (is (nil? (get-in @db* [:analysis c])))
        (is (nil? (get-in @db* [:diagnostics :built-in c]))))
      (finally (fs/delete-tree root)))))

(deftest queued-deletion-preserves-a-new-editor-session
  (let [uri h/default-uri
        db* (h/db*)
        watched (async/chan 2)
        components (assoc (h/components) :watched-files-chan watched)
        cleared* (atom [])]
    (swap! db* assoc :project-root-uri (h/file-uri "file:///"))
    (h/load-code-and-locs "(ns a) (def old 1)" uri)
    (f.file-management/did-change-watched-files [{:uri uri :type :deleted}] components)
    (let [deletion (h/take-or-timeout watched 1000)]
      (h/load-code-and-locs "(ns a) (def unsaved 2)" uri)
      (let [new-session (get-in @db* [:documents uri :open-session])]
        (with-redefs [diagnostics/publish-empty-diagnostics! (fn [uris _] (swap! cleared* into uris))]
          (f.file-management/analyze-watched-files! [deletion] components))
        (is (empty? @cleared*))
        (is (= new-session (get-in @db* [:documents uri :open-session])))
        (is (= "(ns a) (def unsaved 2)" (get-in @db* [:documents uri :text])))
        (is (= 'unsaved (get-in @db* [:analysis uri :var-definitions 0 :name])))
        (f.file-management/did-change-watched-files [{:uri uri :type :created}] components)
        (f.file-management/analyze-watched-files! [(h/take-or-timeout watched 1000)] components)
        (is (= new-session (get-in @db* [:documents uri :open-session])))
        (is (= "(ns a) (def unsaved 2)" (get-in @db* [:documents uri :text])))
        (is (= 'unsaved (get-in @db* [:analysis uri :var-definitions 0 :name])))))))

(deftest closed-files-refresh-text-before-diagnostics
  (doseq [race [:none :reopen :close]]
    (testing (name race)
      (h/reset-components!)
      (let [root (fs/create-temp-dir {:prefix "clojure-lsp-closed-"})
            path (str (fs/path root "a.clj"))
            db* (h/db*)
            uri (shared/filename->uri path @db*)
            old-text "(ns a) #_{:clojure-lsp/ignore [:clojure-lsp/unused-public-var]} (def old 1)"
            disk-text "(ns a) (def fresh 2)"
            editor-text "(ns a) (def unsaved 3)"
            original-kondo kondo/run-kondo-on-text!
            runs* (atom 0)]
        (try
          (swap! db* assoc :project-root-uri (shared/filename->uri (str root) @db*)
                 :settings {:source-paths #{(str root)}})
          (spit path old-text)
          (h/load-code-and-locs old-text uri)
          ;; :close changes the session state while a reference analysis runs;
          ;; the other cases start with an already closed, retained document.
          (when (not= race :close) (f.file-management/did-close uri (h/components)))
          (spit path disk-text)
          (with-redefs [kondo/run-kondo-on-text!
                        (fn [text uri snapshot*]
                          (let [result (original-kondo text uri snapshot*)]
                            (when (= 1 (swap! runs* inc))
                              (case race
                                :reopen (f.file-management/load-document! uri editor-text db*)
                                :close (f.file-management/did-close uri (h/components))
                                nil))
                            result))]
            (if (= race :close)
              (f.file-management/analyze-reference-uris! [uri] db*)
              (f.file-management/analyze-watched-files! [{:uri uri :type :changed}] (h/components))))
          (let [expected (if (= race :reopen) editor-text disk-text)]
            (is (= expected (get-in @db* [:documents uri :text])))
            (is (= expected (z/root-string (parser/safe-zloc-of-file @db* uri))))
            (is (some #(= "clojure-lsp/unused-public-var" (:code %))
                      (get-in @db* [:diagnostics :built-in uri])))
            (is (= (not= race :reopen) (get-in @db* [:documents uri :closed?]))))
          (finally (fs/delete-tree root)))))))

(deftest queued-deletion-validates-the-session-inside-the-atom-update
  (let [uri h/default-uri
        db* (h/db*)
        watched (async/chan 1)
        components (assoc (h/components) :watched-files-chan watched)
        original-remove kondo/db-without-uris
        removals* (atom 0)
        cleared* (atom [])]
    (swap! db* assoc :project-root-uri (h/file-uri "file:///"))
    (h/load-code-and-locs "(ns a) (def old 1)" uri)
    (f.file-management/did-change-watched-files [{:uri uri :type :deleted}] components)
    (let [deletion (h/take-or-timeout watched 1000)]
      (with-redefs [kondo/db-without-uris
                    (fn [state uris]
                      (when (= 1 (swap! removals* inc))
                        ;; Force the atom update to retry against a newer session.
                        (f.file-management/load-document! uri "(ns a) (def unsaved 2)" db*))
                      (original-remove state uris))
                    diagnostics/publish-empty-diagnostics! (fn [uris _] (swap! cleared* into uris))]
        (f.file-management/analyze-watched-files! [deletion] components)))
    (is (empty? @cleared*))
    (is (= "(ns a) (def unsaved 2)" (get-in @db* [:documents uri :text])))
    (is (get-in @db* [:analysis uri]))))
