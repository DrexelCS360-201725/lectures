#lang racket
(require racket/match)

; An environment is a list of bindings.
; A binding is a list with two elements: variable and value
(define initial-env '())

(define (lookup x env)
  (cadr (assq x env)))

(define (extend x val env)
  (cons (list x val) env))

; lambda-eval takes an expression and an environment to a value
; A value is just a closure
(define (lambda-eval e env)
  (match e
    [(? symbol?)     (lookup e env)]
    [`(λ ,v . ,body) `(closure ,e ,env)]
    [`(,f ,arg)      (lambda-apply (lambda-eval f env)
                                   (lambda-eval arg env))]))

; lambda-apply takes a function and an argument to a value
(define (lambda-apply f x)
  (match f
    [`(closure (λ ,v . ,body) ,env)
     (lambda-eval body (extend v x env))]))

(define (top-lambda-eval e)
  (lambda-eval e initial-env))

(module+ test
  (require rackunit)
  (check-equal? (top-lambda-eval '((λ x . x) (λ y . y))) '(closure (λ y . y) ())))

