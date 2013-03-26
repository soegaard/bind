#lang racket
(require "../private/bind.rkt"
         (for-syntax syntax/parse
                     racket/syntax))
(provide (for-syntax :object))

; Semantics:
;   The binding clause
;       [id :object e]
;   will evaluate the expression e and bind the result to id.
;   In the body of bind, applications
;           (id arg ...)
;   will expand to 
;           (send id arg ...).

(define-binding-clause-transformer (:object stx)
  (syntax-parse stx
    [(id:id _ e:expr)
     (values
      (list #'[(id1) e])
      (list #`[id (make-transformer
                   #:context so
                   #:literals (set!)
                   [(set! i e)   (syntax/loc so (set! id1 e))]
                   [(i . more)   (syntax/loc so (send id1 . more))]
                   [i:identifier (syntax/loc so id1)])]))]))

(module* test #f 
  (require rackunit)
  ; Note: It would be ideal for the match api to generate the
  ;       list of variables to be bound. The first example would be:
  ;       (bind ([_ :match (list x _) '(2 3)]) x)
  (define fish%
    (class object%
      (init size)                
      (define current-size size) 
      (super-new)                
      (define/public (get-size) current-size)    
      (define/public (grow amt) (set! current-size (+ amt current-size)))))
  
  (check-equal? (bind ([charlie :object (new fish% [size 10])])
                  (charlie grow 5)
                  (charlie get-size))
                15))




