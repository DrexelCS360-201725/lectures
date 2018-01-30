#lang racket
(require redex)

(define-language λ
  [e ::=
     x
     (e e)
     (λ x e)]
  [v ::= (λ x e)]
  [x y ::= variable-not-otherwise-mentioned])

;; (render-language λ)

;;
;; Capture avoiding substitution
;;
(define-metafunction λ
  subst : x any any -> any
  [(subst x any x)             any]
  [(subst x any_1 (λ x any_2)) (λ x any_2)]
  ;; This is the tricky case; we need to substitute under a lambda and rename
  ;; y to avoid variable capture.
  [(subst x any_1 (λ y any_2)) (λ y_new (subst x any_1 (subst-vars y y_new any_2)))
                               (where y_new ,(variable-not-in (term (x any_1 any_2)) (term y)))]
  [(subst x any_1 (any_2 ...)) ((subst x any_1 any_2) ...)]
  [(subst x any_1 any_2)       any_2])

;;
;; *Non-capture avoiding* substitution. We only use this when we know that the
;; variables we are substituting in is fresh.
;;
(define-metafunction λ
  subst-vars : x x any -> any
  [(subst-vars x_1 y_1 x_1)       y_1]
  [(subst-vars x_1 y_1 (any ...)) ((subst-vars x_1 y_1 any) ...)]
  [(subst-vars x_1 y_1 any)       any])

;;(render-metafunction subst)

;;(render-metafunction subst-vars)

;;
;; Tests for substitution
;;
(test-equal
 (term (subst x (λ z (z w)) (λ y x)))
 (term (λ y (λ z (z w)))))

(test-equal
 (term (subst x y (λ x x)))
 (term (λ x x)))

(test-equal
 (term (subst x z (λ z x)))
 (term (λ z1 z)))

;;
;; Call-by-value single-step and multi-step judgments
;;

(define-judgment-form
  λ
  #:mode (cbv I O)
  #:contract (cbv e_1 e_2)
  
  [(cbv e_1 e_1b)
   --------------------------- "E-App1"
   (cbv (e_1 e_2) (e_1b e_2))]
  
  [(cbv e_2 e_3)
   ---------------------- "E-App2"
   (cbv (v e_2) (v e_3))]
  
  [(cbv ((λ x e) v) (subst x v e)) "E-AppAbs"])

(define-judgment-form
  λ
  #:mode (cbvstar I O)
  #:contract (cbvstar e v)
  [(cbv e_1 e_2) (cbvstar e_2 e_3)
   ----------------- "E-OneOrMore"
   (cbvstar e_1 e_3)]
  
  [(cbvstar v v) "E-Zero"])

;;
;; Pretty-printing
;;

(define (cbv-rewriter lws)
  (match lws
    [(list _ cbv e_1 e_2 _)
     (list "" e_1 " → " e_2)]))

(define (cbvstar-rewriter lws)
  (match lws
    [(list _ cbvstar e_1 e_2 _)
     (list "" e_1 " →* " e_2)]))

(define-syntax-rule (with-rewriters e)
  (with-compound-rewriters
   (['cbv     cbv-rewriter]
    ['cbvstar cbvstar-rewriter])
   e))

(define (cbv-pp v port width text)
  (match v
    [`(cbv ,e_1 ,e_2)     (begin (default-pretty-printer e_1 port width text)
                                 (default-pretty-printer `→ port width text)
                                 (default-pretty-printer e_2 port width text))]
    [`(cbvstar ,e_1 ,e_2) (begin (default-pretty-printer e_1 port width text)
                                 (default-pretty-printer '→* port width text)
                                 (default-pretty-printer e_2 port width text))]
    [_                    (default-pretty-printer v port width text)]))


(define (cbv-derivations e)
  (show-derivations
   #:pp cbv-pp
   (build-derivations
    (cbv ,e e))))

(define (cbvstar-derivations e)
  (show-derivations
   #:pp cbv-pp
   (build-derivations
    (cbvstar ,e e))))

(define omega
  (term ((λ x (x x)) (λ x (x x)))))

; (with-rewriters (render-judgment-form cbv))

; (with-rewriters (render-judgment-form cbvstar))

; (cbv-derivations '(((λ x (λ y (x y))) (λ w w)) (λ z z)))

; (cbvstar-derivations '(((λ x (λ y (x y))) (λ w w)) (λ z z)))