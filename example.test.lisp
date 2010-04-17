#!/usr/local/bin/clisp

(require "example.lisp")
(require "cl-tap-framework.lisp")

(is (fib 1) 1)
(is (fib 2) 1)
(is (fib 3) 2)
(is (fib 4) 3)
(is (fib 5) 5)
(is (fib 6) 8)

(print-test-plan)