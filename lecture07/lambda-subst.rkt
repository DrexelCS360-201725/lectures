#lang racket
(require racket/match)
(require racket/set)

; Find free variables in an expression
(define (fvs e)
  (match e
    [(? symbol?)     (set e)]
    [`(λ ,v . ,body) (set-remove (fvs body) v)]
    [`(,e1 ,e2)      (set-union (fvs e1) (fvs e2))]))

; Substitute s for x in e
(define (subst x s e)
  (match e
    [(? symbol?)  #:when (eq? e x)                     s]
    [(? symbol?)                                       e]
    [`(λ ,y . ,t) #:when (eq? y x)                     e]
    [`(λ ,y . ,t) #:when (not (set-member? (fvs s) y)) `(λ ,y . ,(subst x s t))]
    [`(λ ,y . ,t)                                      (error "Cannot substitute, need to alpha-rename" x s e)]
    [`(,t1 ,t2)                                        `(,(subst x s t1) ,(subst x s t2))]))
  

(module+ test
  (require rackunit)
  (check-equal? (subst 'x '(λ z . (z w)) '(λ y . x)) '(λ y . (λ z . (z w))))
  (check-equal? (subst 'x 'y '(λ x . x)) '(λ x . x)))

; This evaluates to an error: (subst 'x 'z '(λ z . x))

; lambda-eval takes an expression to a value
; A value is a lambda
(define (lambda-eval e)
  (match e
    [(? symbol?)     (error "Free variable" e)]
    [`(λ ,v . ,body) e]
    [`(,f ,arg)      (lambda-apply (lambda-eval f)
                                   (lambda-eval arg))]))

; lambda-apply takes a function and an argument to a value
(define (lambda-apply f s)
  (match f
    [`(λ ,x . ,t) (subst x s t)]))

(module+ test
  (require rackunit)
  (check-equal? (lambda-eval '((λ x . x) (λ y . y))) '(λ y . y)))
