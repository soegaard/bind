#lang racket
(provide #%bind-clause bind
         define-binding-clause-transformer
         (struct-out binding-clause-transformer)
         (for-syntax make-transformer))

(require "clause-transformer.rkt"
         "binding-clause-transformer-struct.rkt")

(begin-for-syntax
  (require "clause-transformer.rkt"
           syntax/parse
           racket/format
           (for-syntax racket/base)
           (only-in unstable/list map2))
  
  (define (expand-clause clause-stx)
    (define (transform-clause bct-id)
      (define transformer (bct-id->transformers bct-id))
      (transformer clause-stx))
    (syntax-parse clause-stx
      [(id-or-ids bct e:expr ...+) (transform-clause #'bct)]
      [(id:id expr:expr)           (transform-clause #'#%bind-clause)]
      [((id:id ...) expr:expr)     (transform-clause #'#%bind-clause)]
      [_ (define msg (~a "bad syntax one [id maybe-bct expr], "
                         "[(id ...) maybe-bct expr] expected, got: "
                         clause-stx))
         (error 'expand-clause msg)]))
  
  (define (expand-clauses clauses-stx)
    ; returns two lists:
    ;  the first is a list of let-values clauses
    ;  the second is a list of let-syntax clauses
    (define-values (cs stx-cs)
      (map2 expand-clause (syntax->list clauses-stx)))
    (values (apply append cs)
            (apply append stx-cs))))

(define-syntax (bind stx)
  (syntax-parse stx
    [(_ () body ...+)
     (syntax/loc stx
       (let-values () body ...))]
    [(_ clauses body ...+)
     (define-values (cs stx-cs) (expand-clauses #'clauses))
     (quasisyntax/loc stx
       (let-values #,cs
         (let-syntax #,stx-cs
           body ...)))]))
