#lang racket
(require racket/trace)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Higher Order Function Examples
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;
; Define a function map that takes a function f and a list xs and returns a new
; list consisting of the results of applying f to each element in xs.
;
; Example:
;  (map (lambda (x) (+ x 1)) '(1 2 3)) => '(2 3 4)
;

(define (map f lst)
  'not-implemented)

;
; Reduce a list of values to a single value by repeatedly application of a binary function. 
;
; Examples:
;   (reduce + 0 '(1 2 3 4)) => 10
;   (reduce * 1 '(1 2 3 4)) => 24
;

(define (reduce f z xs)
  'not-implemented)

; now try it the other way...

;
; Return a list containing only the elements of xs for which the predicate f
; returns true.
;
; Examples:
;   (filter (lambda (x) (> x 0)) '(-1 -4 5 6 0)) => '(5 6)
;

(define (filter f xs)
  'not-implemented)
      
;
; Compute the sum of a list of integers.
;
; Example:
;   (sum '(1 2 3 4)) => 10

;; Write a solution in terms of reduce

(define (sum xs)
  'not-implemented)

;
; Calculate the sum of squares of a list of integers. Make the function non-recursive.
;
; Example:
;   (sum-of-squares '(1 2 3 4 5)) => 55
;

(define (sum-of-squares xs)
  'not-implemented)

; Compute the order of an object (maximum depth)
;  The order of an atom is 0.
;  The order of a list is 1 plus the maximum order of its elements.
;
; Example:
;   (order '(1 (2 (3)) 4)) => 3
;   (order '((()))) => 3
;
; How can we test for an atom?

(define (order l)
  'not-implemented)

;
; Write a non-recursive function that takes a list and returns all the even
; integers in the list

(define (only-even xs)
  'not-implemented)

; Write a function that composes two functions.
; That is, ((compose f g) x) should be the same as (f (g x))

(define (compose f g)
  'not-implemented)

; Write a function that partially applies a function to a single argument.
; That is, ((papply f x) y) should be the same as (f x y)

(define (papply f x)
  'not-implemented)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; State Examples
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (make-withdraw balance)
  (lambda (amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds")))

(define (make-account balance)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch m)
    (cond ((eq? m 'withdraw) withdraw)
          ((eq? m 'deposit) deposit)
          (else (error "Unknown request -- MAKE-ACCOUNT"
                       m))))
  dispatch)
