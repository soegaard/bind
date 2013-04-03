#lang racket
(require "../private/bind.rkt"
         (for-syntax syntax/parse))
(provide :complex)

; Semantics:
;   The binding clause
;       [(x y) :complex e]
;   will evaluate the expression e and bind the real part to
;   x and the imaginary part to y in the body of the 
;   bind expression.

; Examples:
; > (bind ([(x y) :complex (sqrt -4)]) (list x y))
; '(0 2)

(define-binding-clause-transformer (:complex stx)
  (syntax-parse stx
    [[(r:id i:id) _ expr]
     (values
      (list (syntax/loc stx 
              [(r i) (let ([v expr]) 
                       (values (real-part v) (imag-part v)))]))
      '())]))

(module* test #f 
  (require rackunit)
  (check-equal? (bind ([(x y) :complex (sqrt -4)]) (list x y)) '(0 2)))