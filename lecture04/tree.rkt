#lang racket
(require pict)
(require pict/tree-layout)

(provide show-function show-tree)

(define (make-pict x)
  (cond [(symbol? x) (make-pict (symbol->string x))]
        [(null? x)   (text "'()")]
        [(number? x) (make-pict (number->string x))]
        [(string? x) (text x)]
        [else        (error "Cannot make node from" x)]))


(define (make-node x)
  (cc-superimpose (disk #:color "white" 35) (make-pict x)))

(define (make-tree x)
  (if (tree-layout? x)
      x
      (tree-layout #:pict (make-node x))))

(define (show-function f)
  (lambda (x y)
    (tree-layout #:pict (make-node f)
                 (make-tree x)
                 (make-tree y))))

(define (show-tree t)
  (naive-layered t))
