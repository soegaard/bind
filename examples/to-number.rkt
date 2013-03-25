#lang racket
(require "../private/bind.rkt"
         (for-syntax syntax/parse))

; Semantics:
;   The binding clause
;       [id <-number e]
;   will bind id to the result of (to-number e).
;  
; Example:
; (bind ([x <-number 2]
;        [y <-number "32"]
;        [z <-number #\a])
;  (list x y z))
; =>
;   (list 2 32 97)

(define (to-number t)
  (cond
    [(number? t) t]
    [(string? t) (string->number t)]
    [(char? t)   (char->integer t)]
    [else        #f]))

(define-binding-clause-transformer (<-number stx)
  (define msg "expected value, that can be converted to a number")
  (syntax-parse stx
    [(id:id _ expr)
     (values
      (list 
       #`[(id) 
          #,(quasisyntax/loc #'expr
              (or (to-number expr)                    
                  (quasisyntax/loc #'expr
                    (error '<-number (~a #,msg ", got " t)))))])
      '())]))

(module* test #f 
  (require rackunit)
  (check-equal? (bind ([x <-number 2]
                       [y <-number "32"]
                       [z <-number #\a])
                      (list x y z))
                (list 2 32 97))
  (check-exn exn:fail:contract:arity?
             (λ () (bind ([x <-number (values 1 2)]) x))))
