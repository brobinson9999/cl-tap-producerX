#!/usr/bin/clisp

(require "cl-tap-framework.lisp")
(require "pattern-match.lisp")

(is (match '(p ?x b ?y a) '(p ?y b c a))
    '((?Y . C) (?X . ?Y)))

(is (match '(a b c) '(a a a))
    NIL)

(print-test-plan)
