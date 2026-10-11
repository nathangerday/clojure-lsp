(ns clojure-lsp.parser-test
  (:require
   [clojure-lsp.parser :as parser]
   [clojure-lsp.shared :as shared]
   [clojure-lsp.test-helper.internal :as h]
   [clojure.test :refer [are deftest is testing]]
   [rewrite-clj.zip :as z]))

(h/reset-components-before-test)

(deftest shared-document-parsing-test
  (parser/clear-document-cache!)
  (let [uri "file:///parse-cache.clj"
        state (fn [text version] {:documents {uri {:text text :v version}}})
        old (state "[1]" 1)
        new (state "[2]" 2)
        first-root (parser/zloc-of-file old uri)
        new-root (parser/zloc-of-file new uri)]
    (is (identical? new-root (parser/zloc-of-file new uri)))
    (is (= "[1]" (z/root-string (parser/zloc-of-file old uri))))
    (is (identical? new-root (parser/zloc-of-file new uri)))
    (is (= "[1]" (z/root-string first-root)))
    (parser/clear-document-cache! uri)
    (is (= "[3]" (z/root-string (parser/zloc-of-file (state "[3]" 0) uri))))
    (doseq [text ["foo/ " "(:)" "[:foo/]" "(::foo/)"]]
      (parser/clear-document-cache! uri)
      (is (= text (z/root-string (parser/zloc-of-file (state text 0) uri)))))
    (testing "failed parses are not retained"
      (is (thrown? Exception (parser/zloc-of-file (state "[" 1) uri)))
      (is (nil? (get @@#'parser/roots* uri)))
      (is (= "[4]" (z/root-string (parser/zloc-of-file (state "[4]" 2) uri)))))
    (testing "disk-backed text is reread"
      (let [text* (atom "[5]")]
        (with-redefs [shared/slurp-uri (fn [_] @text*)]
          (is (= "[5]" (z/root-string (parser/zloc-of-file {} uri))))
          (reset! text* "[6]")
          (is (= "[6]" (z/root-string (parser/zloc-of-file {} uri))))))))
  (parser/clear-document-cache!))

(deftest concurrent-document-parsing-test
  (parser/clear-document-cache!)
  (let [calls* (atom 0)
        started (promise)
        release (promise)
        parse parser/z-of-string*
        uri "file:///coalesced.clj"
        db {:documents {uri {:text "[1 2 3]" :v 1}}}]
    (with-redefs [parser/z-of-string* (fn [text]
                                        (swap! calls* inc)
                                        (deliver started true)
                                        (deref release 5000 false)
                                        (parse text))]
      (let [requests (doall (repeatedly 8 #(future (parser/zloc-of-file db uri))))]
        (is (true? (deref started 5000 false)))
        (deliver release true)
        (let [roots (mapv #(deref % 5000 ::timeout) requests)]
          (is (= 1 @calls*))
          (is (every? #(identical? (first roots) %) roots))))))
  (parser/clear-document-cache!))

(deftest bounded-document-parsing-test
  (parser/clear-document-cache!)
  (with-redefs [parser/max-cached-documents 2
                parser/max-cached-characters 10]
    (doseq [uri ["a" "b" "c"]]
      (parser/zloc-of-file {:documents {uri {:text "[1]"}}} uri))
    (is (= #{"b" "c"} (set (keys @@#'parser/roots*))))
    (parser/zloc-of-file {:documents {"d" {:text "[1 2 3 4 5]"}}} "d")
    (is (not (contains? @@#'parser/roots* "d"))))
  (parser/clear-document-cache!))

(deftest safe-zloc-of-string
  (are [s] (= s (z/root-string (parser/safe-zloc-of-string s)))
    "(ns foo) foo/bar"
    "(ns foo) foo/ (+ 1 2)"
    "(ns foo) foo/\n(+ 1 2)"
    "(ns foo) (foo/)"
    "(ns foo) :\n"
    "(ns foo) : "
    "(ns foo) (:)"
    "(ns foo)\n:\n"
    "(ns foo) :foo/ (+ 1 2)"
    "(ns foo) (:foo/) (+ 1 2)"
    "(ns foo)\n:foo/\n"
    "(ns foo) ::foo/ (+ 1 2)"
    "(ns foo) (::foo/) (+ 1 2)"
    "(ns foo)\n::foo/\n"))

(defn to-pos [s row col]
  (-> s parser/safe-zloc-of-string (parser/to-pos row col) z/string))

(deftest to-pos-test
  (testing "complete code"
    (let [zloc (parser/safe-zloc-of-string "  foo  ")]
      (is (= "  " (z/string (parser/to-pos zloc 1 1))))
      (is (= 'foo (z/sexpr (parser/to-pos zloc 1 3))))
      (is (= 'foo (z/sexpr (parser/to-pos zloc 1 5))))
      (is (= "  " (z/string (parser/to-pos zloc 1 6))))))
  (testing "incomplete code"
    (testing "entire line"
      (are [col expected] (= expected (to-pos "(ns foo) foo/ (+ 1 2)" 1 col))
        1  "(ns foo)"
        2  "ns"
        3  "ns"
        4  " "
        5  "foo"
        6  "foo"
        7  "foo"
        8  "foo"
        9  " "
        10 "foo/"
        11 "foo/"
        12 "foo/"
        13 "foo/"
        14 " "
        15 "(+ 1 2)"
        16 "+"
        17 " "
        18 "1"
        19 " "
        20 "2"
        21 "2"))
    (is (= "foo/" (to-pos "foo/ " 1 1)))
    (is (= "foo/" (to-pos "foo/\n" 1 1)))
    (is (= "foo/" (to-pos "(foo/)" 1 2)))
    (is (= ":" (to-pos ": " 1 1)))
    (is (= ":" (to-pos ":\n" 1 1)))
    (is (= ":" (to-pos "(:)" 1 2)))
    (is (= ":" (to-pos "[:]" 1 2)))
    (is (= ":" (to-pos "{:}" 1 2)))
    (is (= ":" (to-pos ";; TODO: foo\n{:}" 2 2)))
    (is (= ":" (to-pos ";; TODO: foo\n: " 2 1)))
    (is (= ":" (to-pos "\n:\n" 2 1)))
    (is (= ":foo/" (to-pos ":foo/ " 1 1)))
    (is (= ":foo/" (to-pos ":foo/\n" 1 1)))
    (is (= ":foo/" (to-pos "(:foo/)" 1 2)))
    (is (= ":foo/" (to-pos "\n:foo/\n" 2 1)))
    (is (= "::foo/" (to-pos "::foo/ " 1 1)))
    (is (= "::foo/" (to-pos "::foo/\n" 1 1)))
    (is (= "::foo/" (to-pos "(::foo/)" 1 2)))
    (is (= "::foo/" (to-pos "\n::foo/\n" 2 1)))
    (testing "doesn't impact later nodes' positions"
      (is (= "a" (to-pos "foo/ a" 1 6)))
      (is (= "a" (to-pos ": a" 1 3)))
      (is (= "a" (to-pos ":foo/ a" 1 7)))
      (is (= "a" (to-pos "::foo/ a" 1 8))))
    (testing "on closing brackets we return previous element"
      (is (= "[2 3]" (to-pos "[1 [2 3] 4]" 1 4)))
      (is (= "3" (to-pos "[1 [2 3] 4]" 1 8))))))

(deftest lein-file->edn
  (testing "simple defproject on root"
    (is (= {:foo 1
            :bar 2}
           (parser/lein-zloc->edn (z/of-string "(defproject my-project \"0.1.2\" :foo 1 :bar 2)")))))
  (testing "simple defproject with multiple code before"
    (is (= {:foo 1
            :bar 2}
           (parser/lein-zloc->edn (z/of-string (str "(def otherthing 2)\n"
                                                    "(defproject my-project \"0.1.2\" :foo 1 :bar 2)\n"
                                                    "(def something 1)")))))))
