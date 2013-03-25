#lang racket
(require "../private/bind.rkt"
         (for-syntax syntax/parse))
; This example is very specific.
; Given  (struct point (x y) #:mutable #:prefab)
; the binding clause transformer :point 
; binds id.x, (set! id.x e) and (id.x arg ...),
; where id is bound to a point structure.

; Examples:
; > (bind ([p :point (point 3 4)]) 
;     (list p.x p.y))
; '(3 4)
; > (bind ([p :point (point 3 4)]) 
;     (set! p.x 5)
;     (list p.x p.y))
; '(5 4)

(struct point (x y) #:mutable #:prefab)

(require (for-syntax racket/format
                     racket/syntax))

(define-binding-clause-transformer (:point stx)
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
                  [i:identifier #'(point-y id)])])))]))

#;(begin
    ; The code below must be used to generalize :point
    ; to, say, :dot
    (require (for-syntax racket/struct-info))
    (begin-for-syntax 
      (require racket/match)
      (define (extract-field name name-field)
        (define (to-string stx) (symbol->string (syntax->datum stx)))
        (let ([n  (to-string name)]
              [nf (to-string name-field)])
          (substring nf (+ 1 (string-length n)))))
      (displayln 
       (match (extract-struct-info (syntax-local-value #'point))
         [(list struct:name name name? 
                (list accessors ...) (list setters ...) 
                mutable?)
          (map (λ (a) (extract-field name a)) accessors)]))
      (newline)))

(module* test #f 
  (require rackunit)
  (check-equal? (bind ([p :point (point 3 4)]) (list p.x p.y)) '(3 4)))