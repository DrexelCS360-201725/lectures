#lang racket

;;
;; Add one to a number
;;
(define (inc x)
  (+ 1 x))

;;
;; Church-encoding of zero
;;
(define (zero f x)
  x)

;;
;; Church-encoding of successor
;;
(define (succ n)
  (lambda (f x) (f (n f x))))

;;
;; Define one in terms of zero and succ
;;
(define one
  (succ zero))

;;
;; Define addition
;;
(define (add n m)
  (lambda (f x) (n f (m f x))))

;;
;; A few more numbers...
;;

(define two (succ one))
(define three (succ two))
(define four (succ three))
(define five (succ four))

;;
;; Define multiplication
;;
(define (mult n m)
  (lambda f x (n (lambda (x) (m f x)) x)))
