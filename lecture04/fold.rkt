#lang racket
(require "tree.rkt")

(define (foldr f z xs)
  (if (null? xs)
      z
      (f (car xs) (foldr f z (cdr xs)))))

(define (foldl f z xs)
  (if (null? xs)
      z
      (foldl f (f z (car xs)) (cdr xs))))

;; (show-tree (foldr (show-function 'cons) '() '(1 2 3 4)))
