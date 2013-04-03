#lang racket

(require "../private/bind.rkt"
         (for-syntax syntax/parse))
(provide :match)

; Semantics:
;   The binding clause
;       [id :match pat e]
;   will match the pattern pat against the result of the expression e.
;   The identifier id must appear in the pattern.

;   The binding clause
;       [(id ...) :match pat e]
;   will match the pattern pat against the result of the expression e.
;   All identifiers id ... must appear in the pattern.

; Examples:
; > (bind ([x :match (list x _) '(2 3)]) x)
; 2
; > (bind ([(x y) :match (list x y) '(2 3)]) (+ x y))
; 5
; > (bind ([x :match (list _ _) '(2 3)]) x)
; > x: undefined;

(define-binding-clause-transformer (:match stx)
  (syntax-parse stx
    [(id:id _ pat expr)
     (values
      (list #`[(id) #,(syntax/loc stx (match expr [pat id]))])
      '())]
    [((id:id ...) _ pat expr)
     (values
      (list #`[(id ...) #,(syntax/loc stx (match expr [pat (values id ...)]))])
      '())]))

(module* test #f 
  (require rackunit)
  ; Note: It would be ideal for the match api to generate the
  ;       list of variables to be bound. The first example would be:
  ;       (bind ([_ :match (list x _) '(2 3)]) x)
  (check-equal? (bind ([x :match (list x _) '(2 3)]) x) 2)
  (check-equal? (bind ([(x y) :match (list x y) '(2 3)]) (+ x y)) 5))
