#lang racket
(provide #%bind-clause
         bct-id->transformers
         define-binding-clause-transformer
         (for-syntax make-transformer))

(require syntax/parse
         "binding-clause-transformer-struct.rkt")

(define (bct-id->transformers bct-id-stx)
    (define maybe-bct (eval bct-id-stx))
    (unless (binding-clause-transformer? maybe-bct)
      (error 'bct-id->transformer
             "expected binding clause transformer"))
    (binding-clause-transformer-transformer maybe-bct))

(define #%bind-clause
    (binding-clause-transformer
     '#%bind-clause
     (λ (stx)
       (syntax-parse stx
         [(id:id expr:expr)
          (values (list (syntax/loc stx [(id) expr])) '())]
         [((id:id ...) expr:expr)
          (values (list (syntax/loc stx [(id ...) expr])) '())]
         [_ 
          (define msg 
            (~a "expected [id expr] or [(id ...) expr], got: " stx))
          (error '#%bind-clause msg)]))))

(begin-for-syntax
  (require (for-syntax syntax/parse)
           (for-syntax (for-template syntax/parse))
           (for-syntax racket/base)
           (for-syntax (for-template "binding-clause-transformer-struct.rkt")))
  (provide make-transformer)  
  (define-syntax (make-transformer s)
    (syntax-parse s
      [(_ (~seq #:context ctx)
          (~optional (~seq #:literals (~and lits (lit ...)))       
                     #:defaults ([lits #'()]))
          [pat expr] ...)
       (with-syntax ([(lit ...) 
                      (for/list ([l (syntax->list (attribute lits))])
                        (datum->syntax s l))])
         #`(make-set!-transformer
            (λ (ctx)
              (syntax-parse ctx
                #:datum-literals (lit ...)
                [pat expr] ...
                [(i . more) (syntax/loc ctx (#%app i . more))]
                [i:identifier (syntax/loc ctx ctx)]
                [_ #'(error 'make-transformer) ]))))])))

(define-syntax (define-binding-clause-transformer stx)
  (syntax-parse stx
    [(_ bct-id:id transformer-expr)
     (syntax/loc stx
       (begin-for-syntax
         (define bct-id
           (binding-clause-transformer 
            #'id transformer-expr))))]
    [(_ (bct-id:id arg:id) transformer-expr ...)
     (syntax/loc stx
       (define-binding-clause-transformer bct-id
         (λ (arg) transformer-expr ...)))]))
    

    