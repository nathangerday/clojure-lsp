(ns clojure-lsp.feature.file-management
  (:require
   [babashka.fs :as fs]
   [clojure-lsp.clj-depend :as lsp.depend]
   [clojure-lsp.config :as config]
   [clojure-lsp.feature.completion-lib :as f.completion-lib]
   [clojure-lsp.feature.diagnostics :as f.diagnostic]
   [clojure-lsp.feature.diagnostics.built-in :as f.diagnostics.built-in]
   [clojure-lsp.feature.diagnostics.custom :as f.diagnostics.custom]
   [clojure-lsp.feature.rename :as f.rename]
   [clojure-lsp.kondo :as lsp.kondo]
   [clojure-lsp.parser :as parser]
   [clojure-lsp.producer :as producer]
   [clojure-lsp.queries :as q]
   [clojure-lsp.settings :as settings]
   [clojure-lsp.shared :as shared]
   [clojure.core.async :as async]
   [clojure.java.io :as io]
   [clojure.set :as set]
   [clojure.string :as string]))

(set! *warn-on-reflection* true)

(defn- analysis-inputs [db]
  {:settings (settings/all db)
   :project-root-uri (:project-root-uri db)
   :kondo-config (:kondo-config db)
   :kondo-config-hash (:kondo-config-hash db)
   :classpath (:classpath db)
   :analysis (:analysis db)
   :dep-graph (:dep-graph db)})

(defn- same-analysis-inputs? [a b]
  ;; Compare the potentially huge analysis by identity: a dependency/source
  ;; analysis change conservatively requires a fresh run. Cursor, progress,
  ;; document bookkeeping and other unrelated updates do not.
  (and (identical? (:analysis a) (:analysis b))
       (identical? (:dep-graph a) (:dep-graph b))
       (= (dissoc a :analysis :dep-graph) (dissoc b :analysis :dep-graph))))

(defn- commit-analysis!
  "Calculate against a snapshot, reusing work only while its inputs remain valid.
  The CAS only publishes a prepared state; expensive linters never run in swap!."
  [db* {:keys [uris analyze valid? finish custom? documents?]
        :or {valid? (constantly true) finish identity}}]
  (loop [state-db @db* previous nil]
    (when (valid? state-db)
      (let [inputs (analysis-inputs state-db)
            same-inputs? (and previous (same-analysis-inputs? inputs (:inputs previous)))
            same-documents? (identical? (:documents state-db) (:documents previous))
            reusable? (and same-inputs? (or (not documents?) same-documents?))
            results (if reusable? (:results previous) (analyze (atom state-db)))
            analyzed-uris (or (:uris results) uris)
            candidate (cond-> (reduce lsp.kondo/db-with-results state-db (:kondo results))
                        (contains? results :depend) (lsp.depend/db-with-results (:depend results)))
            candidate (reduce-kv (fn [db uri text]
                                   (assoc-in db [:documents uri :text] text))
                                 candidate (:closed-texts results))
            diagnostics (if (and reusable? same-documents?)
                          (:diagnostics previous)
                          {:built-in (case (count analyzed-uris)
                                       0 {}
                                       1 (f.diagnostics.built-in/analyze-uri! (first analyzed-uris) candidate)
                                       (f.diagnostics.built-in/analyze-uris! analyzed-uris candidate))
                           :custom (when custom?
                                     (reduce (fn [diags uri]
                                               (merge diags (f.diagnostics.custom/analyze-uri! uri candidate)))
                                             {} analyzed-uris))})
            candidate (cond-> (update-in candidate [:diagnostics :built-in] merge (:built-in diagnostics))
                        custom? (update-in [:diagnostics :custom] merge (:custom diagnostics)))
            candidate (finish candidate)]
        (if (compare-and-set! db* state-db candidate)
          {:before state-db :after candidate :uris analyzed-uris}
          (recur @db* {:inputs inputs
                       :documents (:documents state-db)
                       :results results
                       :diagnostics diagnostics}))))))

(defn- analyze-document [uri text snapshot*]
  (let [kondo-result* (future
                        (shared/logging-task
                          :internal/uri-analyzed-by-clj-kondo
                          (lsp.kondo/run-kondo-on-text! text uri snapshot*)))
        depend-result* (future
                         (shared/logging-task
                           :internal/uri-analyzed-by-clj-depend
                           (lsp.depend/analyze-uri! uri @snapshot*)))]
    {:kondo [@kondo-result*] :depend @depend-result*}))

(defn- current-document? [db uri text version session]
  (let [document (get-in db [:documents uri])]
    (and (= version (:v document))
         (= session (:open-session document))
         (not (:closed? document))
         (or (nil? (:text document)) (= text (:text document))))))

(defn create-ns-changes [uri text db]
  (when-let [new-ns (and (string/blank? text)
                         (shared/uri->safe-namespace uri db))]
    (when (settings/get db [:auto-add-ns-to-new-files?] true)
      (let [new-text (format "(ns %s)" new-ns)
            changes [{:text-document {:version (get-in db [:documents uri :v] 0) :uri uri}
                      :edits [{:range (shared/->range {:row 1 :end-row 999999 :col 1 :end-col 999999})
                               :new-text new-text}]}]]
        (shared/client-changes changes db)))))

(defn load-document! [uri text db*]
  ;; Reset both versions when reopening too: cached analysis belongs to the
  ;; previous document session. A negative marker makes requests wait for open.
  (let [session (parser/start-document-cache! uri)]
    (swap! db* update-in [:documents uri] assoc :v 0 :analyzed-version -1
           :text text :saved-on-disk false :open-session session :closed? false)))

(defn did-open [uri text {:keys [db* producer edits-chan] :as components} allow-create-ns]
  (let [session (get-in (load-document! uri text db*) [:documents uri :open-session])]
    (when (commit-analysis! db* {:uris [uri]
                                 :analyze #(analyze-document uri text %)
                                 :valid? #(current-document? % uri text 0 session)
                                 :custom? true
                                 :finish #(assoc-in % [:documents uri :analyzed-version] 0)})
      (f.diagnostic/publish-diagnostics! uri components)
      (when allow-create-ns
        (when-let [create-ns-edits (create-ns-changes uri text @db*)]
          (async/>!! edits-chan create-ns-edits)))))
  (when (and (f.completion-lib/dep-file? uri)
             (not (:libs @f.completion-lib/libs*))
             (not (:api? @db*)))
    (producer/with-work-done-progress producer "Fetching libs for completion" f.completion-lib/fetch-libs!)))

(defn ^:private set-xor [a b]
  (into (set/difference a b)
        (set/difference b a)))

(defn ^:private find-changed-elems
  "Detect elements that went from 0 to 1 occurrence, or from 1 to 0
  occurrences."
  [old-local-buckets new-local-buckets bucket signature-fn]
  (let [signature-with-elem (fn [elem]
                              (with-meta (signature-fn elem) {:elem elem}))
        old-signs (into #{} (map signature-with-elem) (bucket old-local-buckets))
        new-signs (into #{} (map signature-with-elem) (bucket new-local-buckets))]
    (map (comp :elem meta)
         (set-xor old-signs new-signs))))

(def ^:private definition-signature
  "Attributes of a var definition that, if they change, merit re-linting files
  that use the var. This isn't the full list of attributets copied by kondo from
  defs to usages, but it includes the attributes we care about."
  (juxt :ns :name :private :fixed-arities :defined-by :defined-by->lint-as))

(defn reference-uris [uri db-before db-after]
  (let [old-local-buckets (get-in db-before [:analysis uri])
        new-local-buckets (get-in db-after [:analysis uri])
        changed-var-definitions (find-changed-elems old-local-buckets new-local-buckets :var-definitions definition-signature)
        changed-var-usages (find-changed-elems old-local-buckets new-local-buckets :var-usages q/var-usage-signature)
        changed-kw-usages (find-changed-elems old-local-buckets new-local-buckets :keyword-usages q/kw-signature)
        project-db (-> db-after
                       q/db-with-internal-analysis
                       ;; don't notify self
                       (update :analysis dissoc uri))
        var-dependent-uri (when (seq changed-var-definitions)
                            (let [def-signs (into #{}
                                                  (mapcat q/var-definition-signatures)
                                                  changed-var-definitions)
                                  usage-of-changed-def? (fn [var-usage]
                                                          (contains? def-signs (q/var-usage-signature var-usage)))]
                              (into #{}
                                    (keep (fn [[uri {:keys [var-usages]}]]
                                            (when (some usage-of-changed-def? var-usages)
                                              uri)))
                                    (q/uri-dependents-analysis project-db uri))))
        var-dependency-uris (when (seq changed-var-usages)
                              ;; If definition is in both a clj and cljs file, and this is a clj
                              ;; usage, we are careful to notify only the clj file. But, it wouldn't
                              ;; really hurt to notify both files. So, if it helps readability,
                              ;; maintenance, or symmetry with `var-dependent-uris`, we could look
                              ;; at just signature, not signature and lang.
                              (let [usage-signs->langs (->> changed-var-usages
                                                            (reduce (fn [result usage]
                                                                      (assoc result (q/var-usage-signature usage) (q/elem-langs usage)))
                                                                    {}))
                                    def-of-changed-usage? (fn [var-def]
                                                            (when-let [usage-langs (some usage-signs->langs
                                                                                         (q/var-definition-signatures var-def))]
                                                              (some usage-langs (q/elem-langs var-def))))]
                                (into #{}
                                      (keep (fn [[uri {:keys [var-definitions]}]]
                                              (when (some def-of-changed-usage? var-definitions)
                                                uri)))
                                      (q/uri-dependencies-analysis project-db uri))))
        kw-dependency-uris (when (seq changed-kw-usages)
                             (let [usage-signs (into #{}
                                                     (map q/kw-signature)
                                                     changed-kw-usages)
                                   def-of-changed-usage? (fn [kw-def]
                                                           (contains? usage-signs (q/kw-signature kw-def)))]
                               (into #{}
                                     (keep (fn [[uri {:keys [keyword-definitions]}]]
                                             (when (some def-of-changed-usage? keyword-definitions)
                                               uri)))
                                     (:analysis project-db))))]
    ;; TODO: see note on `notify-references` We may want to handle these
    ;; sets of files differently.
    (set/union (or var-dependent-uri #{})
               (or var-dependency-uris #{})
               (or kw-dependency-uris #{}))))

(defn- analyze-files [uris snapshot* required-analysis-uris]
  (let [db @snapshot*
        uris (into [] (comp (distinct)
                            (filter #(or (not (contains? required-analysis-uris %))
                                         (contains? (:analysis db) %)))
                            (filter #(let [document (get-in db [:documents %])]
                                       (or (and (not (:closed? document)) (:text document))
                                           (shared/file-exists? (io/file (shared/uri->filename %)))))))
                   uris)
        document-texts (into {} (keep (fn [uri]
                                        (when-let [document (get-in db [:documents uri])]
                                          (when-let [text (if (:closed? document)
                                                            (shared/slurp-uri uri)
                                                            (:text document))]
                                            [uri text])))) uris)
        closed-texts (into {} (filter (fn [[uri _]] (get-in db [:documents uri :closed?]))) document-texts)
        disk-uris (into [] (remove #(contains? document-texts %)) uris)]
    {:uris uris
     ;; Refresh retained closed text before diagnostics/parser lookups. Analyze
     ;; that same text, so ignore comments and kondo see the same file contents.
     :closed-texts closed-texts
     :kondo (cond-> (mapv (fn [[uri text]] (lsp.kondo/run-kondo-on-text! text uri snapshot*)) document-texts)
              (seq disk-uris) (conj (lsp.kondo/run-kondo-on-reference-uris! disk-uris snapshot*)))}))

(defn analyze-reference-uris!
  "Re-analyze references once, using current editor text and validated snapshots."
  [uris db*]
  ;; Acquire the gate in the caller: never create a pool of tasks waiting for it.
  ;; Other DB writers remain free to run; commit-analysis! validates their changes.
  (locking db*
    (let [db @db*
          required-analysis-uris (into #{} (filter #(contains? (:analysis db) %)) uris)]
      ;; Explicit callers may also request disk files not analyzed yet. Only
      ;; previously known targets must remain present during publication retries.
      (commit-analysis! db* {:uris uris :documents? true
                             :analyze #(analyze-files uris % required-analysis-uris)}))))

(defn ^:private notify-references [changed-uris db-before db-after {:keys [db* producer] :as components}]
  (when (settings/get db-after [:notify-references-on-file-change] true)
    (shared/logging-task
      :internal/notify-references
      (let [uris (shared/logging-task
                   :reference-files/find
                   (into #{}
                         (comp
                           (mapcat #(set/union (reference-uris % db-before db-after)
                                               (reference-uris % db-after db-before)))
                           ;; The reverse lookup finds old namespaces after deletion/rename.
                           ;; Never reintroduce targets absent from the final analysis.
                           (filter #(contains? (:analysis db-after) %)))
                         changed-uris))]
        (when (seq uris)
          (let [result (shared/logging-task
                         :reference-files/analyze
                         (analyze-reference-uris! uris db*))]
            (f.diagnostic/publish-all-diagnostics! (:uris result) true components)
            (producer/refresh-code-lens producer)))))))

(defn ^:private offsets [lines line character end-line end-character]
  (loop [lines (seq lines)
         offset 0
         idx 0]
    (if (or (not lines)
            (= line idx))
      [(+ offset character line)
       (loop [lines lines
              offset offset
              idx idx]
         (if (or (not lines)
                 (= end-line idx))
           (+ offset end-character end-line)
           (recur (next lines)
                  (+ offset (count (first lines)))
                  (inc idx))))]
      (recur (next lines)
             (+ offset (count (first lines)))
             (inc idx)))))

(defn replace-text
  "Returns ORIGINAL text but replacing its substring from LINE number at
  CHARACTER position to END-LINE number at END-CHARACTER position with
  REPLACEMENT text.

  All line endings of the returned string are replaced with that of
  the ORIGINAL's line ending type.

  The ORIGINAL's line ending type is set to be its first line ending
  type (either `\r\n` or `\n`), or, if no newline is found, the
  system's line separator."
  [original replacement line character end-line end-character]

  (let [original-nl (or (re-find #"\r\n|\n" original)
                        shared/line-separator)
        replacement-nl (re-find #"\r\n|\n" replacement)
        ;; normalize `\r\n`'s to `\n` so that `\r`'s do not creap in
        ;; as normal chars
        original (cond-> original
                   (= original-nl "\r\n") (string/replace #"\r\n" "\n"))
        replacement (cond-> replacement
                      (= replacement-nl "\r\n") (string/replace #"\r\n" "\n"))
        lines (string/split-lines original) ;; don't use OS specific line delimiter!
        [start-offset end-offset] (offsets lines line character end-line end-character)
        [prefix suffix] [(subs original 0 start-offset)
                         (subs original (min end-offset (count original)) (count original))]]
    ;; restore normalized string to ORIGINAL's line ending type.
    (cond-> (string/join [prefix replacement suffix])
      (= original-nl "\r\n") (string/replace #"\n" "\r\n"))))

(defn ^:private handle-change
  "Handle a TextDocumentContentChangeEvent"
  [old-text change]
  (let [new-text (:text change)]
    (if-let [r (:range change)]
      (let [{{start-line :line
              start-character :character} :start
             {end-line :line
              end-character :character} :end} r]
        (replace-text old-text new-text start-line start-character end-line end-character))
      ;; If range and rangeLength are omitted the new text is considered to be
      ;; the full content of the document.
      new-text)))

(defn ^:private bump-version
  "Advance the analyzed-version, but only if it's newer, in case analysis
  completes out of order."
  [old-version new-version]
  (if (or (not old-version)
          (< old-version new-version))
    new-version
    old-version))

(defn analyze-changes [{:keys [uri text version open-session]} {:keys [producer db*] :as components}]
  (when-let [{:keys [before after]}
             (commit-analysis! db* {:uris [uri]
                                    :analyze #(analyze-document uri text %)
                                    :valid? #(current-document? % uri text version open-session)
                                    :custom? true
                                    :finish #(update-in % [:documents uri :analyzed-version]
                                                        bump-version version)})]
    (f.diagnostic/publish-diagnostics! uri components)
    (notify-references [uri] before after components)
    (producer/refresh-test-tree producer [uri])))

(defn did-change [uri changes version {:keys [db* current-changes-chan]}]
  (let [state-db (swap! db* (fn [state-db]
                              (let [text (reduce handle-change (get-in state-db [:documents uri :text]) changes)]
                                (-> state-db
                                    (assoc-in [:documents uri :v] version)
                                    (assoc-in [:documents uri :text] text)))))
        document (get-in state-db [:documents uri])]
    (async/>!! current-changes-chan {:uri uri
                                     :text (:text document)
                                     :version version
                                     :open-session (:open-session document)})))

(defn- document-state [db uri]
  (select-keys (get-in db [:documents uri]) [:open-session :v]))

(defn- remove-files!
  ([uris components] (remove-files! uris {} components))
  ([uris expected-states {:keys [db*] :as components}]
   (let [eligible (fn [db]
                    (filterv #(or (not (contains? expected-states %))
                                  (= (get expected-states %) (document-state db %))) uris))
         [before _] (swap-vals! db* (fn [db]
                                      (let [removed (eligible db)]
                                        (-> (lsp.kondo/db-without-uris db removed)
                                            (update-in [:diagnostics :built-in] #(apply dissoc % removed))
                                            (update-in [:diagnostics :custom] #(apply dissoc % removed))))))
         removed (eligible before)]
     ;; Only an applied deletion may clear diagnostics. A queued old deletion
     ;; must not erase a newer editor session or its unsaved text.
     (f.diagnostic/publish-empty-diagnostics! removed components)
     removed)))

(defn ^:private files-deleted [old-db {:keys [db*] :as components} uris]
  (let [removed (remove-files! uris components)]
    (notify-references removed old-db @db* components)))

(defn analyze-watched-files!
  "Apply the final event per URI, then analyze the union of affected references."
  [changes {:keys [db* producer] :as components}]
  (let [old-db @db*
        uri->event (into {} (map (juxt :uri identity)) changes)
        deleted (into [] (keep (fn [[uri event]] (when (= :deleted (:type event)) uri))) uri->event)
        expected-states (into {} (keep (fn [[uri event]]
                                         (when (contains? event :document-state)
                                           [uri (:document-state event)]))) uri->event)
        surviving (into [] (comp (remove (fn [[_ event]] (= :deleted (:type event)))) (map key)) uri->event)
        removed (when (seq deleted) (remove-files! deleted expected-states components))]
    (when (seq surviving)
      (let [result (commit-analysis! db* {:uris surviving :documents? true
                                          :analyze #(analyze-files surviving % #{})})]
        (f.diagnostic/publish-all-diagnostics! (:uris result) true components)
        (producer/refresh-test-tree producer (:uris result))))
    ;; Synchronous completion supplies backpressure to the watched-files consumer.
    (notify-references (concat removed surviving) old-db @db* components)))

(defn ^:private dir-or-file-uri->analyzable-uris [uri db]
  ;; If the URI is for an entire directory that has been created/deleted, we
  ;; expand it.
  (let [source-paths-ignore-regexs (get-in db [:settings :source-paths-ignore-regex] config/default-source-path-ignore-regexs)
        root-path (shared/uri->path (:project-root-uri db))
        files (-> uri shared/uri->filename io/file file-seq)]
    (into []
          (comp
            ;; avoid watching ignored source-paths
            (remove (fn [file]
                      (let [relative-source-path (shared/relativize-filepath (str file) (str root-path))]
                        (some #(re-matches (re-pattern %) relative-source-path) source-paths-ignore-regexs))))
            (map #(.getAbsolutePath ^java.io.File %))
            (map #(shared/filename->uri % db))
            (remove #(identical? :unknown (shared/uri->file-type %))))
          files)))

(defn did-change-watched-files
  [changes {:keys [db* watched-files-chan]}]
  (let [db @db*
        observe-changed? (settings/get db [:compute-external-file-changes] true)]
    (doseq [{:keys [uri type]} changes
            :when (or observe-changed? (not= :changed type))
            analyzable-uri (dir-or-file-uri->analyzable-uris uri db)]
      (async/>!! watched-files-chan {:uri analyzable-uri :type type
                                     :document-state (document-state db analyzable-uri)}))))

(defn did-close [uri {:keys [db*] :as components}]
  ;; Keep cached text/analysis and the retired session's parse-cache protection,
  ;; but let pending requests distinguish closing from ordinary version updates.
  (swap! db* (fn [db]
               (if (get-in db [:documents uri])
                 (assoc-in db [:documents uri :closed?] true)
                 db)))
  (parser/clear-document-cache! uri)
  (let [db @db*
        filename (shared/uri->filename uri)
        source-paths (settings/get db [:source-paths])
        external-filename? (shared/external-filename? filename source-paths)]
    (if external-filename?
      (f.diagnostic/publish-empty-diagnostics! [uri] components)
      (when-not (shared/uri-on-disk? uri)
        (files-deleted db components [uri])))))

(defn force-get-document-text
  "Get document text from db, if document not found, tries to open the document"
  [uri {:keys [db*] :as components}]
  (or (get-in @db* [:documents uri :text])
      (when-let [text (shared/slurp-uri uri)]
        (did-open uri text components false)
        (get-in @db* [:documents uri :text]))))

(defn ^:private lint-opened-files-when-any-config-changed!
  [uri {:keys [db* producer] :as components}]
  (let [db @db*
        project-root-filename (shared/uri->filename (:project-root-uri db))
        config-files #{(io/file project-root-filename ".clj-kondo" "config.edn")
                       (io/file project-root-filename ".lsp" "config.edn")}
        config-file-saved? (some (comp #(= uri %)
                                       #(shared/filename->uri % db)
                                       str
                                       fs/canonicalize)
                                 config-files)]

    (when config-file-saved?
      (let [all-opened-uris (->> (:documents db)
                                 (keep (fn [[uri document]] (when (:v document) uri)))
                                 set)]
        (analyze-reference-uris! all-opened-uris db*)
        (f.diagnostic/publish-all-diagnostics! all-opened-uris true components)
        (producer/refresh-code-lens producer)))))

(defn did-save [uri {:keys [db*] :as components}]
  (swap! db* #(assoc-in % [:documents uri :saved-on-disk] true))
  (lint-opened-files-when-any-config-changed! uri components))

(defn will-rename-files [files db]
  (->> files
       (keep (fn [{:keys [old-uri new-uri]}]
               (let [new-ns (shared/uri->namespace new-uri db)
                     old-ns-definition (q/find-namespace-definition-by-uri db old-uri)]
                 (when (and new-ns
                            old-ns-definition
                            (not= new-ns (name (:name old-ns-definition))))
                   (f.rename/rename-element new-ns db old-ns-definition :rename-file)))))
       (reduce #(shared/deep-merge %1 %2) {:document-changes []})))

(defn did-rename-files [files {:keys [db*] :as components}]
  (files-deleted @db* components (mapv :old-uri files)))
