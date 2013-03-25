#lang racket
(require "../private/bind.rkt"
         (for-syntax syntax/parse))
(provide (for-syntax :delay))

; Semantics:
;   The binding clause
;       [id :delay e]
;   will bind id to the result of evaluting the expression (delay e).
;   In the body of bind the following bindings are in place:
;      reference:    id           returns  (force id)
;      assignment:   (set! id f)  as normal
;      application   (id arg ...) returns ((force id) arg ...)

; Examples:
; > (bind ([x :delay (/ 1 0)]) 3)
; 3
; > (bind ([x :delay 3]) (+ x x))
; 6
; > (bind ([x :delay (/ 1 0)]) (set! x 4) (+ x x))
; 8

(define-binding-clause-transformer (:delay stx)
  (syntax-parse stx
    [(id:id _ expr)
     (values
      (list #`[(id) #,(syntax/loc stx (delay expr))])
      (list 
       #`[id (make-transformer
              #:context so
              #:literals (set!)
              [(set! i e)   (syntax/loc so (set! id e))]
              [(i . more)   (syntax/loc so (#%app (force id) . more))]
              [i:identifier (syntax/loc so (force id))])]))]))

(module* test #f 
  (require rackunit)
  (check-equal? (bind ([x :delay (/ 1 0)]) 3) 3)
  (check-exn exn:fail:contract:divide-by-zero? 
             (λ () (bind ([x :delay (/ 1 0)]) x)))
  (check-equal? (bind ([x :delay 3]) (+ x x)) 6)
  (check-equal? (bind ([x :delay (/ 1 0)]) (set! x 4) (+ x x)) 8))