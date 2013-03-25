#lang racket
(require "../private/bind.rkt"
         (for-syntax syntax/parse))

(define-binding-clause-transformer (:floor stx)
  (syntax-parse stx #:literals (values)
    [(id:id _ expr)
     (values (list #'[(id) (exact-floor expr)]) '())]
    [((id:id ...) _ (values expr ...))
     (values (list (syntax/loc stx
                     [(id ...) (values (exact-floor expr) ...)]))
             '())]
    [((id:id ...) _ expr)
     (with-syntax ([(t ...) (generate-temporaries #'(id ...))])
       (values (list (quasisyntax/loc stx
                       [(id ...) 
                        #,(syntax/loc #'expr
                            (let-values ([(t ...) expr])
                              (values (exact-floor t) ...)))]))
               '()))]
    [_ (error ':int "binding clause expected")]))

(module* test #f 
  (require rackunit)
  (check-equal? (bind ([x :floor 2.5]) x) 2)
  (check-equal? (bind ([x :floor 2.5] [y :floor 3.7]) (+ x y)) 5)
  (check-equal? (bind ([(x y) :floor (values 2.5 3.7)]) (+ x y)) 5)
  (check-exn exn:fail:contract:arity?
             ; two values expected, list produces only one
             (λ () (bind ([(x y) :floor (list 2.5 3.7)]) (+ x y))))
  
  ; This should (and does) give a duplicate binding name error:
  ; (bind ([y :floor 3.7] [y :floor 3.7]) y)
  )
