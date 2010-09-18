#!/usr/bin/clisp

(require "cl-tap-framework.lisp")

(deftest test-arithmetic ()
  (combine-results
   (test-+)
   (test-*)))

(deftest test-+ ()
  (check
    (= (+ 1 2) 3)
    (= (+ 1 2 3) 6)
    (= (+ -1 -3) -4)))

(deftest test-* ()
  (check
   (= (* 2 2) 4)
   (= (* 3 5) 15)))

(print-test-list (test-arithmetic))
