#lang racket
(define-syntax time-exp
  (syntax-rules ()
    ((_ e ...)
     (begin (collect-garbage)
            (time (begin e ...))
            #t))))

;;
;; Tail recursion
;;

;
; Count the number of numbers from 0 to n
;
; Try (count-exp 1000000)
;
(define (count-to n)
  (if (= n 0)
      1
      (+ 1 (count-to (- n 1)))))

(define (count-to-tail n)
  (define (count-to-aux n acc)
    (if (= n 0)
        acc
        (count-to-aux (- n 1) (+ 1 acc))))
  (count-to-aux n 0))

;
; Compute factorial of n
; 
(define (fact n)
  'not-implemented)

(define (fact-tail n)
  'not-implemented)

; Compute the nth Fibonacci number
; F_0 = 0
; F_1 = 1
; F_2 = 1
; ...
; F_n = F_{n-1} + F_{n-2}
;
; The first few numbers are:
;    0, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, 144, 233, 377, 610
;
; Example:
;   (map fib '(0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15)) => (0 1 1 2 3 5 8 13 21 34 55 89 144 233 377 610)

(define (fib n)
  'not-implemented)

(define (fib-tail n)
  'not-implemented)
