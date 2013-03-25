#lang racket
(require "../private/bind.rkt"
         (for-syntax syntax/parse
                     racket/syntax))
(provide (for-syntax :vector))

; Semantics:
;   The binding clause
;       [id :vector n e]
;   will evaluate the expression e. The resulting value v must
;   be a vector of length n. 
;   Then id0, id1, ... will be bound to the n elements in v.
;   Furthermore (set! vi e) will expand to (vector-set! vi e).


(define-binding-clause-transformer (:vector stx)
  (syntax-parse stx
    [(id:id _ n:exact-nonnegative-integer expr)
     (let ([n (syntax-e #'n)])
       (with-syntax ([(idi ...)
                      (for/list ([i n])
                        (format-id #'id "~a~a" #'id i))]
                     [(j ...) (for/list ([j n]) j)])
         (values
          (list #'[(id) expr])
          (map (λ (idi j)
                 (with-syntax ([idi idi] [j j])
                   #`[idi (make-transformer
                           #:context so
                           #:literals (set!)
                           [(set! i e)   (syntax/loc so (vector-set! id j e))]
                           [(i . more)   (syntax/loc so (#%app idi . more))]
                           [i:identifier (syntax/loc so (vector-ref id j))])]))
               (syntax->list #'(idi ...))
               (syntax->list #'(j ...))))))]))
          
(module* test #f 
  (require rackunit)
  ; Note: It would be ideal for the match api to generate the
  ;       list of variables to be bound. The first example would be:
  ;       (bind ([_ :match (list x _) '(2 3)]) x)
  (check-equal? (bind ([v :vector 2 (vector 3 4)])
                      (set! v1 5)
                      (+ v0 v1))
                8))


      
       
