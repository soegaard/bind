#lang racket
(require "../private/bind.rkt"
         (for-syntax syntax/parse
                     racket/syntax))
(provide (for-syntax :string :string/idx))

; Semantics:
;   The binding clause
;       [s :string e]
;   will evaluate the expression e and bind the result to s.
;   In the body of the bind expression:
;       (s i)     expands to (string-ref s i)
;       (s! i x)  expands to (string-set! s i x)
;        s!       expands to (λ (i x) (vector-set! s i x))

(define-binding-clause-transformer (:string stx)
  (syntax-parse stx
    [(v:id _ e:expr)
     (values
      (list #'[(v*) e])
      (with-syntax ([v! (format-id #'v "~a!" #'v)])
        (list #`[v (make-transformer
                    #:context so
                    #:literals (set!)
                    [(set! w e)   (syntax/loc so (set! v* i e))]
                    [(w k)        (syntax/loc so (string-ref v* k))]
                    [(w k l)      (syntax/loc so (substring v* k l))]
                    [(w . more)   (syntax/loc so (#%app v* . more))]
                    [i:identifier (syntax/loc so v*)])]
              #`[v! (make-transformer
                     #:context so
                     #:literals (set!)
                     [(w! . more)   (syntax/loc so (string-set! v* . more))]
                     [w!:identifier (syntax/loc so (λ (i x) (string-set! v* i x)))])])))]))

; Semantics:
;   The binding clause
;       [s :string/idx n e]
;   will evaluate the expression e and bind the result to v.
;   In the body of the bind expression the n identifiers
;        s0, s1, ...
;   will expand to (string-ref s i).
;   Furthermore (set! si e) will expand to (string-set! s i e).

(define-binding-clause-transformer (:string/idx stx)
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
                        [(i k)        (syntax/loc so (string-ref id* k))]
                        [(i . more)   (syntax/loc so (#%app id* . more))]
                        [i:identifier (syntax/loc so id*)])])
           (map (λ (idi j)
                  (with-syntax ([idi idi] [j j])
                    #`[idi (make-transformer
                            #:context so
                            #:literals (set!)
                            [(set! i e)   (syntax/loc so (string-set! id* j e))]
                            [(i . more)   (syntax/loc so (#%app idi . more))]
                            [i:identifier (syntax/loc so (string-ref id* j))])]))
                (syntax->list #'(idi ...))
                (syntax->list #'(j ...)))))))]))

(module* test #f 
  (require rackunit)
  ; Note: It would be ideal for the match api to generate the
  ;       list of variables to be bound. The first example would be:
  ;       (bind ([_ :match (list x _) '(2 3)]) x)
  (check-equal? (bind ([s :string (string #\a #\b)])
                  (s! 0 #\c) 
                  (string (s 0) (s 1)))
                "cb")
  ; :vector/idx is cute, but is it useful?
  (check-equal? (bind ([s :string/idx 2 (string #\a #\b)])
                  (set! s1 #\c)
                  (string s0 s1))
                "ac"))
