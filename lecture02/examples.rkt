#lang racket

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; List Examples
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Compute the length of the list l
;   The length of the empty list is 0.
;   The length of a non-empty list is 1 + the length of the cdr of the list.
;
; Examples:
;   (length '()) => 0
;   (length '(1 2)) => 2
;   (length '(1 (2 3))) => 2
;

(define (length l)
  'not-implemented)

;
; Compute the sum of a list of integers
;
; Example:
;   (sum '(1 2 3 4)) => 10

(define (sum xs)
  'not-implemented)

; Return the nth element of a list, counting from 0.
;
; Examples:
;  (nth 0 '(1 2 3)) => 1
;  (nth 2 '(1 2 3)) => 3

(define (nth n l)
  'not-implemented)

; Concatenate the lists l1 and l2 (append l2 to l1)
;   The concatention of l1 and l2 is equal to l2 if l1 is null.
;   Otherwise it is the list whose first element (car) is the first
;   element of l1 and whose tail (cdr) is equal to the concatention
;   of the tail of l1 and l2.
;
; Example:
;   (concat '(1 2 3) '(4 5 6)) => (1 2 3 4 5 6)
;

(define (concat l1 l2)
  'not-implemented)

; Count the number of integers in a list
;  The number of integers in an object that is an integer is 1.
;  The number of integers in a null list is zero.
;  Otherwise the number of integers in a list l is the number of integers
;    in the car of l plus the number of integers in the cdr of l.
;
; Example:
;   (numints 5) => 1
;   (numints '(1 2 (3 4) 5)) => 5
;
; How can we handle nested lists that contain non-integers?

(define (numints l)
  'not-implemented)

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
; Calculate the sum of squares of a list of integers. Make the function non-recursive.
;
; Example:
;   (sum-of-squares '(1 2 3 4 5)) => 55
;

(define (sum-of-squares xs)
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
