#lang racket
(require "../private/bind.rkt"
         (for-syntax syntax/parse
                     racket/syntax))
(provide (for-syntax :vector :vector/idx))

; Semantics:
;   The binding clause
;       [v :vector e]
;   will evaluate the expression e and bind the result to v.
;   In the body of the bind expression:
;       (v i)     expands to (vector-ref v i)
;       (v i j)   expands to (vector-copy v i j)
;       (v! i x)  expands to (vector-set! v i x)
;        v!       expands to (λ (i x) (vector-set! v i x))

(define-binding-clause-transformer (:vector stx)
  (syntax-parse stx
    [(v:id _ e:expr)
     (values
      (list #'[(v*) e])
      (with-syntax ([v!       (format-id #'v "~a!" #'v)])
        (list #`[v (make-transformer
                    #:context so
                    #:literals (set!)
                    [(set! w e)   (syntax/loc so (set! v* i e))]
                    [(w k)        (syntax/loc so (vector-ref v* k))]
                    [(w k l)      (syntax/loc so (vector-copy v* k l))]
                    [(w . more)   (syntax/loc so (#%app v* . more))]
                    [i:identifier (syntax/loc so v*)])]
              #`[v! (make-transformer
                     #:context so
                     #:literals (set!)
                     [(w! . more)   (syntax/loc so (vector-set! v* . more))]
                     [w!:identifier (syntax/loc so (λ (i x) (vector-set! v* i x)))])])))]))

; Semantics:
;   The binding clause
;       [id :vector/idx n e]
;   will evaluate the expression e. The resulting value v must
;   be a vector of length n. 
;   Then id0, id1, ... will be bound to the n elements in v.
;   Furthermore (set! vi e) will expand to (vector-set! vi e).

(define-binding-clause-transformer (:vector/idx stx)
  (syntax-parse stx
    [(id:id _ n:exact-nonnegative-integer expr)
     (let ([n (syntax-e #'n)])
       (with-syntax ([(idi ...)
                      (for/list ([i n])
                        (format-id #'id "~a~a" #'id i))]
                     [(j ...) (for/list ([j n]) j)])
         (values
          (list #'[(id*) expr])
          (append
           (list #`[id (make-transformer
                        #:context so
                        #:literals (set!)
                        [(set! i e)   (syntax/loc so (set! id* i e))]
                        [(i k)        (syntax/loc so (vector-ref id* k))]
                        [(i . more)   (syntax/loc so (#%app id* . more))]
                        [i:identifier (syntax/loc so id*)])])
           (map (λ (idi j)
                  (with-syntax ([idi idi] [j j])
                    #`[idi (make-transformer
                            #:context so
                            #:literals (set!)
                            [(set! i e)   (syntax/loc so (vector-set! id* j e))]
                            [(i . more)   (syntax/loc so (#%app idi . more))]
                            [i:identifier (syntax/loc so (vector-ref id* j))])]))
                (syntax->list #'(idi ...))
                (syntax->list #'(j ...)))))))]))

(module* test #f 
  (require rackunit)
  ; Note: It would be ideal for the match api to generate the
  ;       list of variables to be bound. The first example would be:
  ;       (bind ([_ :match (list x _) '(2 3)]) x)
  (check-equal? (bind ([v :vector (vector 3 4)])
                  (v! 0 5) 
                  (list (v 0) (v 1)))
                (list 5 4))
  ; :vector/idx is cute, but is it useful?
  (check-equal? (bind ([v :vector/idx 2 (vector 3 4)])
                  (set! v1 5)
                  (+ v0 v1))
                8))
