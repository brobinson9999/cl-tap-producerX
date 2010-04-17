; fib: nat -> nat
; purpose: produces the nth element of the Fibonacci sequence
; examples:
;   (fib 1) => 1
;   (fib 2) => 1
;   (fib 3) => 2
;   (fib 4) => 3
(defun fib (n)
  (cond
    ((<= n 2) 1)
    (T (+ (fib (- n 1)) (fib (- n 2))))))
