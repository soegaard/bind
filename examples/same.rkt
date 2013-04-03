#lang racket
(require "../private/bind.rkt"
         (for-syntax syntax/parse))
(provide :same)

; Semantics:
;   The binding clause
;       [(id ...) :same e]
;   will evaluate the expression e and bind the
;   result value to the identifiers id ....
; Example:
;    (bind ([(x y) :same 1]) (list x y))  
; => (list 1 1)

(define-binding-clause-transformer (:same stx)
  (syntax-parse stx #:literals (values)
    [((id:id ...) _ expr)
     (let* ([ids (syntax->list #'(id ...))]
            [ts  (map (λ(_) #'t) ids)])
       (values (list (quasisyntax/loc stx
                       [(id ...) 
                        #,(quasisyntax/loc #'expr
                            (let ([t expr])
                              (values  #,@ts)))]))
               '()))]
    [_ (error ':int "binding clause expected")]))

(module* test #f 
  (require rackunit)
  ; :same  bind the same value to several identifiers
  (check-true (bind ([(x y) :same (list 1 2 3)]) (eq? x y)))
  )