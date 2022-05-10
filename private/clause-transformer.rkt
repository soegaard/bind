#lang racket
(provide #%bind-clause
         bct-id->transformers
         define-binding-clause-transformer
         (for-syntax make-transformer))

(require syntax/parse
         "binding-clause-transformer-struct.rkt")

(begin-for-syntax
  (require syntax/parse racket/format
           (for-syntax syntax/parse racket/base ))
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

(define (bct-id->transformers bct-id-stx)
  (define maybe-bct (syntax-local-value bct-id-stx (λ()#f)))
  (unless (binding-clause-transformer? maybe-bct)
    (error 'bct-id->transformer
           "expected binding clause transformer"))
  (binding-clause-transformer-transformer maybe-bct))


(require (for-syntax "binding-clause-transformer-struct.rkt"))           

(define-syntax (define-binding-clause-transformer stx)
  (syntax-parse stx
    [(_ bct-id:id transformer-expr)
     (syntax/loc stx
       (define-syntax bct-id
         (binding-clause-transformer #'id transformer-expr)))]
    [(_ (bct-id:id arg:id) transformer-expr ...)
     (syntax/loc stx
       (define-binding-clause-transformer bct-id
         (λ (arg) transformer-expr ...)))]))

; (define-conventions name-id convention-rule ...)
; where
;   convention-rule = (name-pattern syntax-class)
;   name-pattern = exact-id | name-rx
; 	syntax-class = syntax-class-id | (syntax-class-id expr ...)

; (define-binding-conventions name-id convention-rule ...)
; where
;   convention-rule = (name-pattern bct-id)
;   name-pattern = name-rx
(require (for-syntax racket/syntax))
#;(define-syntax (define-binding-conventions stx)
  (syntax-parse stx
    [(convs-name:id [rx bct-id:id] ...)
     (define/with-syntax rules #'([rx bct-id] ...))
     (define/with-syntax (rule ...) #'rules)
     (define/with-syntax 
       (parser ...)
       #'((λ (name)
            (define str (symbol->string name))
            (if (regexp-match rx str)
                #'bct-id
                #f))
          ...))
     #'(begin
         (define-syntax convs-name
           (make-binding-conventions
            (quote-syntax get-parsers) ; get-procedures
            (lambda ()                 ; get-rules
              (let ([bct-ids (list (quote-syntax bct-id) ...)])
                (map list
                     (list 'rx ...)
                     (map make-den:delayed
                          (generate-temporaries bct-ids) ; parser
                          bct-ids))))))                  ; class
         (define get-parsers
           (lambda formals
             (list parser ...))))]))

(define-binding-clause-transformer
  #%bind-clause
  (λ (stx)
    ; (displayln '#%bind-clause)
    (syntax-parse stx
      [(id:id _ expr:expr)
       (values (list (syntax/loc stx [(id) expr])) '())]
      [((id:id ...) _ expr:expr)
       (values (list (syntax/loc stx [(id ...) expr])) '())]
      [_ 
       (define msg 
         (~a "expected [id expr] or [(id ...) expr], got: " stx))
       (error '#%bind-clause msg)])))


  
