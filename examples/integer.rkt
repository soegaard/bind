#lang racket
(require "../private/bind.rkt"
         racket/syntax
         (for-syntax syntax/parse racket/syntax))
(provide :integer :int)

; :int is shorthand for :integer

; Semantics:
;   The binding clause
;       [id :integer e]
;   will bind id to the result of evaluting the expression e.
;   In the body of bind the following bindings are in place:
;      (id! e)   expands to (set! id e)
;      (id+! x)  expands to (begin (set! id (+ id x)) id)
;      (id-! x)  expands to (begin (set! id (- id x)) id)
;      (id*! x)  expands to (begin (set! id (* id x)) id)
;      id++      expands to (begin (set! id (+ id 1)) id)

; Examples:
; > (bind ([x :int 3]) x++)
; 4
; > (bind ([x :int 3]) (x+! 2) x)
; 5

(define-binding-clause-transformer (:integer stx)
  (syntax-parse stx
    [(i*:id _ e:expr)
     (define (make-id format-str) (format-id #'i* format-str #'i*))
     (with-syntax ([i      (generate-temporary #'i*)]
                   [i!     (make-id "~a!")]
                   [i+!    (make-id "~a+!")]
                   [i-!    (make-id "~a-!")]
                   [i*!    (make-id "~a*!")]
                   [i++    (make-id "~a++")]
                   [in-i   (make-id "in-~a")]
                   [from-i (make-id "from-~a")]
                   [i+1    (make-id "~a+1")]
                   [i-1    (make-id "~a-1")])       
       (define (make-arith-transformer name op)
         (with-syntax ([name name] [op op])
           #`[name (make-transformer
                    #:context so #:literals (set!)
                    [(_ e1:expr) 
                     (syntax/loc so 
                       (begin (set! i (op i e1))
                              i))])]))
       (define (make-short name ex)
         (with-syntax ([name name] [ex ex])
           #`[name (make-transformer
                    #:context so #:literals (set!)
                    [_:id  (syntax/loc so ex)])]))
       (values
        (list 
         #`[(i) #,(syntax/loc stx e)])
        (list 
         #`[i*     (make-transformer
                    #:context so #:literals (set!)
                    [(set! _ e:expr) (syntax/loc so (set! i e))]
                    [_:id            (syntax/loc so i)])]
         #`[i!     (make-transformer
                    #:context so #:literals (set!)
                    [(_ x:expr) (syntax/loc so (set! i x))])]
         #`[i++    (make-transformer
                    #:context so #:literals (set!)
                    [_:id (syntax/loc so (begin (set! i (add1 i)) i))])]
         #`[in-i   (make-transformer
                    #:context so #:literals (set!)
                    [_:id (syntax/loc so (in-range i))])]
         #`[from-i (make-transformer
                    #:context so #:literals (set!)
                    [_:id (syntax/loc so (in-naturals i))])]
         (make-arith-transformer #'i+! #'+)
         (make-arith-transformer #'i-! #'-)
         (make-arith-transformer #'i*! #'*)
         (make-short #'i+1 #'(add1 i))
         (make-short #'i-1 #'(sub1 i)))))]))

(define-syntax :int (make-rename-transformer #':integer))

(module* test #f 
  (require rackunit)
  (check-equal? (bind ([x :integer 3]) (x! 4) x) 4)
  (check-equal? (bind ([x :int 3]) x++) 4)
  (check-equal? (bind ([x :int 3]) x++ x) 4)
  (check-equal? (bind ([x :int 3]) (x+! 1) x) 4)
  (check-equal? (bind ([x :int 3]) (for/list ([y in-x]) y)) (list 0 1 2))
  (check-equal? (bind ([x :int 3]) (for/list ([y from-x] [z 2]) y)) (list 3 4))
  (check-equal? (bind ([x :int 3]) x+1) 4)
  (check-equal? (bind ([x :int 3]) x-1) 2))
