#!/usr/local/bin/clisp

(require "cl-tap-framework.lisp")

; we want to do white-box testing in here, so we want to access functions
; that are not exported. We could use cl-tap-producerX::foo but instead
; I will just set the cl-tap-producerX as the current package for convenience.
(in-package :cl-tap-producerX)

(is (add-to-plist-if-nonexistent :a 'b '()) '(:a b))
(is (add-to-plist-if-nonexistent :a 'b '(:c d)) '(:a b :c d))
(is (add-to-plist-if-nonexistent :a 'b '(:a d)) '(:a d))
(is (add-to-plist-if-nonexistent :a 'b '(:c d :a NIL)) '(:c d :a NIL))

(is (is-func NIL NIL) `(:test-passed T :test-value NIL :expected-value NIL))
(is (is-func 5 5) `(:test-passed T :test-value 5 :expected-value 5))
(is (is-func "5" "5") `(:test-passed T :test-value "5" :expected-value "5"))
(is (is-func "5" "5" :test-name "check if 5=5") `(:test-passed T :test-value "5" :expected-value "5" :test-name "check if 5=5"))
(is (is-func 5 5) `(:test-passed T :test-value 5 :expected-value 5))

(is (report-result T `example-form) `(:test-name NIL :raw-test example-form :test-passed T))
(is (report-result `(:test-passed T) `example-form) `(:test-name NIL :raw-test example-form :test-passed T))

(is (check T)
    `((:test-name NIL :raw-test T :test-passed T)))
(is (check T T)
    `((:test-name NIL :raw-test T :test-passed T)
      (:test-name NIL :raw-test T :test-passed T)))
(is (check (is-func 1 1))
    `((:test-name NIL :raw-test (is-func 1 1) :test-passed T :test-value 1 :expected-value 1)))
(is (check (is-func 1 1) (is-func 2 2))
    `((:test-name NIL :raw-test (is-func 1 1) :test-passed T :test-value 1 :expected-value 1)
      (:test-name NIL :raw-test (is-func 2 2) :test-passed T :test-value 2 :expected-value 2)))

(defun always-true (dummy-a dummy-b) T)
(is 5 6 :compare-sym 'always-true)
(is 5 6 :compare-fun (lambda (x y) T))

(is (is 5 5) '(:test-passed T :test-value 5 :expected-value 5 :raw-test (is 5 5)))

(let ((my-condition (make-condition 'error)))
  (is (check-equivalence my-condition my-condition) T))
(is (check-equivalence (make-condition 'error) (make-condition 'error)) T)
(is (check-equivalence (make-condition 'error) (make-condition 'condition)) NIL)

(is (capture-condition (+ 5 3)) NIL)
(is (capture-condition (+ 5 3) (* 10 3)) NIL)
(let ((my-condition (make-condition 'error)))
  (is (capture-condition (signal my-condition)) my-condition))

(is (capture-condition (error 'error)) (make-condition 'error))
(is (capture-condition (+ 5 3) (error 'error) (* 10 3)) (make-condition 'error))

(is-condition (+ 1 3) NIL)
(is-condition (error 'error) (make-condition 'error))

(print-test-plan)