#lang racket
(require "../private/bind.rkt"
         (for-syntax syntax/parse
                     racket/syntax
                     racket/stream))

(provide (for-syntax :hash))

(define-binding-clause-transformer (:hash stx)
  (syntax-parse stx
    [(id*:id _ e*:expr)
     (define (make-id format-str) (format-id #'id* format-str #'id*))
     (with-syntax ([id   (generate-temporary #'id*)])
       (values
        (list #'[(id) e*])
        (list #`[id* (make-transformer
                      #:context so #:literals (set!)
                      [(set! _ e)     (syntax/loc so (set! id e))]
                      [(_ k)          (syntax/loc so (hash-ref id k))]
                      [(_ k d)        (syntax/loc so (hash-ref id k d))]
                      [_:identifier   (syntax/loc so id)])])))]))

(module* test #f
  (require rackunit racket/block)
  (block
   (def h :hash (hash 'a 1 'b 2))
   (check-equal? (h 'a) 1)
   (check-equal? (h 'c 'not-found) 'not-found)))
   
  

