#lang racket
(require "../private/bind.rkt"
         (for-syntax syntax/parse
                     racket/base
                     racket/syntax
                     racket/format))


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

(define-binding-clause-transformer :dot
  (λ (stx)
    (define (id.field id field)
      (format-id id (~a (syntax->datum id) "." field)))
    (syntax-parse stx
      [(id:id _ expr)
        (with-syntax
            ([id.x (id.field #'id 'x)]
             [id.y (id.field #'id 'y)])
          (values
           (list #'[(id) expr])
           (list 
            #`[id.x (make-transformer
                     #:context so #:literals (set!)
                     [(set! i e)   (syntax/loc so (set-point-x! id e))]
                     [(i . more)   (syntax/loc so (#%app (point-x id) . more))]
                     [i:identifier #'(point-x id)])]
            #`[id.y (make-transformer
                     #:context so #:literals (set!)
                     [(set! i e)  (syntax/loc so (set-point-y! id e))]
                     [(i . more)  (syntax/loc so (#%app (point-y id) . more))]
                     [i:identifier #'(point-y id)])])))])))

(module* test #f 
  (require rackunit)
  
  )
