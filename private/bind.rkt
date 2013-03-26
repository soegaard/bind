#lang racket
(provide #%bind-clause bind def
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
      [_ (displayln clause-stx)
       (define msg (~a "bad syntax one [id maybe-bct expr], "
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

(define-syntax (def stx)
  (syntax-parse stx
    [(_ name:id e:expr)
     (syntax/loc stx
       (define name expr))]
    [(_ (name:id ...) e:expr)
     (syntax/loc stx
       (define-values (name ...) expr))]
    [(_ name:id bct:id . more)
     (define-values (cs stx-cs) (expand-clauses #'([name bct . more])))
     (with-syntax ([((id e) ...) cs]
                   [((stx-id stx-e) ...) stx-cs])
     (syntax/loc stx
       (begin
         (define-values id e) ...
         (define-syntax stx-id stx-e) ...)))]
    [(_ (name:id ...) bct:id . more)
     (define-values (cs stx-cs) (expand-clauses #'([(name ...) bct . more])))
     (with-syntax ([((id e) ...) cs]
                   [((stx-id stx-e) ...) stx-cs])
     (syntax/loc stx
       (begin
         (define-values id e) ...
         (define-syntax stx-id stx-e) ...)))]))
