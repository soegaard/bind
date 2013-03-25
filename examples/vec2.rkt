#lang racket
(require "../private/bind.rkt"
         (for-syntax syntax/parse
                     racket/syntax))
(provide (for-syntax :vec2))

; Semantics:
;   The binding clause
;       [id :vec2 e]
;   will evaluate the expression e. The resulting value v must
;   be a vector of length 2. 
;   Then id0 and id1 will be bound to the elements in v.

; Examples:
; > (bind ([x :delay (/ 1 0)]) 3)
; 3
; > (bind ([x :delay 3]) (+ x x))
; 6
; > (bind ([x :delay (/ 1 0)]) (set! x 4) (+ x x))
; 8

(define-binding-clause-transformer (:vec2 stx)
  (syntax-parse stx
    [(id:id _ expr)
     (with-syntax ([id0 (format-id #'id "~a0" #'id)]
                   [id1 (format-id #'id "~a1" #'id)])
       (values
        (list #'[(id id0 id1) 
                 (let ([v expr])
                   (values v
                           (vector-ref v 0)
                           (vector-ref v 1)))])
        (list)))]))

(bind ([v :vec2 #(3 4)])
      (+ v0 v1))

      
       
