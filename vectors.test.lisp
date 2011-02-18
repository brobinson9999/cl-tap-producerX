#!/usr/bin/clisp

(load "cl-tap-framework.lisp")

; vectors
(is (vector) #())
(is (vector 1) #(1))
(is (vector 1 2) #(1 2))
(is (vector (vector 1 2) (vector 3 4)) #(#(1 2) #(3 4)))
(is (vector (vector 1 2) (vector 3 4)) #(#(1 2) #(3 4)) :compare-sym 'equalp)

(print-test-plan)
