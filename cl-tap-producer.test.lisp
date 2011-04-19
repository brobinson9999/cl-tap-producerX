#!/usr/bin/clisp

;(require "example.lisp")
(require "cl-tap-framework.lisp")

; we want to do white-box testing in here, so we want to access functions
; that are not exported. We could use cl-tap-producerX::foo but instead
; I will just set the cl-tap-producerX as the current package for convenience.
(in-package :cl-tap-producerX)

(deftest embedded-tests ()
  (check
   (starts-with-whitespace-p " test")
   (starts-with-whitespace-p "  test")
   (starts-with-whitespace-p " example")
   (starts-with-whitespace-p " this is a test")
   (not (starts-with-whitespace-p "test"))
   (not (starts-with-whitespace-p "# test"))
   (not (starts-with-whitespace-p "\nt   est"))
   (not (starts-with-whitespace-p ""))
   (is (start-line-with-whitespace "test" 0) "test")
   (is (start-line-with-whitespace "test" 1) " test")
   (is (start-line-with-whitespace "sample" 1) " sample")
   (is (start-line-with-whitespace "example" 2) "  example")
   (is (start-line-with-whitespace " test" 2) "  test")
   (is (start-line-with-whitespace "  test" 2) "  test")
   (is (start-line-with-whitespace "   test" 2) "   test")
   (is (start-line-with-whitespace "test" 3) "   test")
   (is (start-line-with-whitespace "" 3) "   ")
   (is (start-line-with-whitespace "test" 2 :always-add-whitespace NIL) "  test")
   (is (start-line-with-whitespace "test" 2 :always-add-whitespace T) "  test")
   (is (start-line-with-whitespace "  test" 2 :always-add-whitespace NIL) "  test")
   (is (start-line-with-whitespace "  test" 2 :always-add-whitespace T) "    test")

   (is (start-all-lines-with-whitespace "") "  ")
   (is (start-all-lines-with-whitespace
	    (format NIL "line1~%line2")) (format NIL "  line1~%  line2"))
   (is (start-all-lines-with-whitespace
	    (format NIL "  line1~%   line2")) (format NIL "  line1~%   line2"))
   (is (start-all-lines-with-whitespace
	    (format NIL "    line1~%line2")) (format NIL "    line1~%  line2"))
   (is (start-all-lines-with-whitespace
	    (format NIL "    line1~%~%line3")) (format NIL "    line1~%  ~%  line3"))

   (is (plist-contains-p '(:test "value") :test) T)
   (is (plist-contains-p '(:rest "value" :test "value") :test) T)
   (is (plist-contains-p '(:cat 5 :test 7 :rest "value") :test) T)
   (is (plist-contains-p '() :test) NIL)
   (is (plist-contains-p '(:rest "value") :test) NIL)
   (is (plist-contains-p '(:cat 5 :rest "value") :test) NIL)

   (is (structured-test->tap '(:test-passed T)) "ok 1")
   (is (structured-test->tap '(:test-passed T)) "ok 2")
   (is (structured-test->tap '()) "not ok 3")
   (is (get-test-plan) "1..3")
   (is (structured-test->tap '(:test-passed T :test-name "test")) "ok 4 test")
   (is (structured-test->tap '(:test-name "test" :todo-desc "not done")) "not ok 5 test # TODO not done")
   (is (get-test-plan) "1..5")
   (is (structured-test->tap '(:skip-desc "skipped")) "not ok 6 # skip skipped")
   (is (structured-test->tap '(:todo-desc "todox" :skip-desc "skipped")) "not ok 7 # skip skipped # TODO todox")
   (is (get-test-plan) "1..7")

   (is (get-diagnostic-text `(:test-value "foundthis")) "found: \"foundthis\"")
   (is (get-diagnostic-text `(:expected-value "wantedthis")) "wanted: \"wantedthis\"")
   (is (get-diagnostic-text `(:test-value "foundthis" :expected-value "wantedthis"))
       (format NIL "found: \"foundthis\"~%wanted: \"wantedthis\""))

   (is (format-test-value NIL) "NIL")
   (is (format-test-value 5) "5")
   (is (format-test-value "test") "\"test\"")

   (is (structured-test->tap '(:test-passed T :test-name "test" :test-value T :expected-value T))
       (format NIL "ok 8 test~%  found: T~%  wanted: T"))
   (is (structured-test->tap '(:test-passed NIL :test-name "test" :test-value T :expected-value T))
       (format NIL "not ok 9 test~%  found: T~%  wanted: T"))
   (is (structured-test->tap '(:test-passed NIL :test-name "test" :test-value T :expected-value NIL))
       (format NIL "not ok 10 test~%  found: T~%  wanted: NIL"))
   (is (structured-test->tap '(:test-passed NIL :test-name "test" :test-value NIL :expected-value T))
       (format NIL "not ok 11 test~%  found: NIL~%  wanted: T"))
   (is (structured-test->tap '(:test-passed NIL :test-name "test" :test-value NIL))
       (format NIL "not ok 12 test~%  found: NIL"))
   (is (structured-test->tap '(:test-passed NIL :test-name "test" :expected-value T))
       (format NIL "not ok 13 test~%  wanted: T"))
   (is (structured-test->tap '(:test-passed T :raw-test (dummy test "meow")))
       (format NIL "ok 14~%  raw_test: (DUMMY TEST \"meow\")"))
))

; reset the test counter before actually printing the results, since the tests we executed
; while testing will have advanced the counter.
(setf *test-results* (embedded-tests))
(setf *report-test-index* 0)

(print-test-list *test-results*)
