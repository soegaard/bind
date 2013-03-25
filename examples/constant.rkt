#lang racket
(require "../private/bind.rkt"
         (for-syntax syntax/parse))

; Semantics:
;   The binding clause
;       [id :constant e]
;   will bind id to the result of evaluting the expression e.
;   Furthermore the syntax (set! id expr) will result in
;   an "constant not assignable" error.
;  
; Examples:
; > (bind ([x :constant 2]) x)
; 2
; > (bind ([x :constant 2]) (set! x 3) x)
; constant: constant not assignable (c colored red)

(define-binding-clause-transformer :constant
  (λ (stx)
    (syntax-parse stx
      [[id:id _ expr]
       (values
        (list (syntax/loc stx      [(v) expr]))
        (list (quasisyntax/loc stx [id (make-constant-transformer #'v)])))])))

(begin-for-syntax
  (define (make-constant-transformer expr)
    (make-transformer
     #:context so
     #:literals (set!)
     [(set! i e) 
      (syntax/loc #'i (error 'constant "constant not assignable"))]     
     [id:id expr])))

(module* test #f 
  (require rackunit)
  (check-equal? (bind ([x :constant 2]) x) 2)
  (check-equal? (bind ([foo :constant (λ (x) (+ x 1))]) (foo 3)) 4)
  ; ok (check-exn exn:fail? (λ () (bind ([x :constant 2]) (set! x 3) x)))
  )