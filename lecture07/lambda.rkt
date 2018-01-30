#lang racket

; A lambda has the form (λ var . body).
; It is a dotted pair!
(define lambda-var cadr)
(define lambda-body cddr)

; A closure is a pair of code and environment.
; We represent code directly as a lambda!
(define closure-code car)
(define closure-env cdr)

(define (make-closure code env)
  (cons code env))

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
  (cond
    [(symbol? e)       (lookup e env)]
    [(eq? (car e) 'λ)  (make-closure e env)]
    [else              (lambda-apply (lambda-eval (car e) env)
                                     (lambda-eval (cadr e) env))]))

; lambda-apply takes a function and an argument to a value
(define (lambda-apply f x)
  (lambda-eval (lambda-body (closure-code f))
               (extend (lambda-var (closure-code f))
                       x
                       (closure-env f))))

(define (top-lambda-eval e)
  (lambda-eval e initial-env))

(module+ test
  (require rackunit)
  (check-equal? (top-lambda-eval '((λ x . x) (λ y . y))) '((λ y . y))))

