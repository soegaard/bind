#lang racket
(require bind racket/stream
         (for-syntax syntax/parse
                     racket/syntax
                     racket/stream))

;;; FUNCTIONAL PEARLS
;;; Power Series, Power Serious
;;; M. Douglas McIlroy

; The code in this file is inspired by the functional pearl.
; The paper uses Haskell.

(define-binding-clause-transformer (:stream stx)
  (syntax-parse stx
    [(id*:id _ e*:expr)
     (define (make-id format-str) (format-id #'id* format-str #'id*))
     (with-syntax ([id   (generate-temporary #'id*)]
                   [id0  (make-id "~a0")]
                   [ids  (make-id "~as")])
       (values
        (list #'[(id) e*])
        (list #`[id* (make-transformer
                      #:context so #:literals (set!)
                      [(set! i e)     (syntax/loc so (set! id e))]
                      [(i 0)          (syntax/loc so (stream-first id))]
                      [(i (~datum _)) (syntax/loc so (stream-rest id))]
                      [(i k)          (syntax/loc so (stream-ref id k))]
                      [i:identifier   (syntax/loc so id)])]
              #`[id0 (make-transformer
                      #:context so #:literals (set!)
                      [i:identifier (syntax/loc so (stream-first id))])]
              #`[ids (make-transformer
                      #:context so #:literals (set!)
                      [i:identifier (syntax/loc so (stream-rest id))])])))]))

(define-syntax :: 
  (syntax-id-rules () [(:: e0 e1s) (stream-cons e0 e1s)]))

(define (negate f)
  (with ([f :stream])
    (:: (- f0) (negate fs))))

(define (plus f g)
  (with ([f g :stream])
    (:: (+ f0 g0) 
        (plus fs gs))))

(define (minus f g)
  (with ([f g :stream])
    (:: (- f0 g0) 
        (minus fs gs))))

(define (scalar-mult s f)
  (with ([f :stream])
    (:: (* s f0)
        (scalar-mult s fs))))

(define (mult f g)
  (with ([f g :stream])
    (:: (* f0 g0)
        (plus (scalar-mult f0 gs)
              (mult fs g)))))

(define (sqr f)
  (mult f f))

(define (div f g)
  (with ([f g :stream])
    (if (= f0 g0 0)
        (div fs gs)
        (let ([q (/ f0 g0)])
          (:: q (div (minus fs (scalar-mult q gs))
                     g))))))

(def ps0 :stream (:: 0 ps0))
(def ps1 :stream (:: 1 ps0))
(def psx :stream (:: 0 (:: 1 ps0)))

(define (pow f n)
  (cond 
    [(< n 0) (error)]
    [(= n 0) (:: 1 ps0)]
    [(= n 1) f]
    [(= n 2) (sqr f)]
    [(even? n) (sqr (pow f (/ n 2)))]
    [(odd? n)  (mult f (pow f (- n 1)))]))

(define (compose f g)
  (with ([f g :stream])
    (unless (= g0 0)
      (error 'compose 
             "first element of the second power series is non-zero"))
    (:: f0 (mult gs (compose fs (:: 0 gs))))))

(define (revert f)
  (with ([f :stream])
    (unless (= f0 0)
      (error 'revert 
             "first element of power series is non-zero"))
    (def rs :stream (:: 0 (div ps1 (compose fs rs))))
    rs))

(define (deriv f)
  (with ([f :stream])
    (define (deriv1 g n)
      (with ([g :stream] [n :int])
        (:: (* n g0) (deriv1 gs n+1))))
    (deriv1 fs 1)))

(define (integral f)
  (define (int1 g n)
    (with ([g :stream] [n :int])
      (:: (/ g0 n) (int1 gs n+1))))
  (:: 0 (int1 f 1)))
    

(define (take n f)
  (with ([f :stream])
    (for/list ([i n]) 
      (f i))))

(define-syntax-rule (hold f) (stream-rest (:: 0 f)))

(def expx :stream (hold (plus ps1 (integral expx))))  ;; Haskell lazy, so no hold
(def sinx :stream (hold (integral cosx)))             ;;  "
(def cosx :stream (hold (minus ps1 (integral sinx)))) ;;  "

(define (square-root f)
  (with ([f :stream])
    (cond [(= f0 0 (f 1))
           (def g :stream fs)
           (:: 0 (square-root gs))]
          [(= f0 1)
           (def q :stream 
             (plus ps1 (integral 
                        ; integral produces 1 + ...
                        ; so we can hold its argument                        
                        (hold (div (deriv f)
                                   (scalar-mult 2 q))))))
           q]
          [else (error 'square-root "hmm...")])))
        

(def y :stream (pow (minus (:: 1 ps0) (scalar-mult 2 (sqr psx))) 3))
(take 10 y)
; 1/(10 - x)
(take 10 (div ps1 (minus ps1 psx)))
(take 10 (div ps1 (sqr (minus ps1 psx))))
(take 10 (deriv (div ps1 (minus ps1 psx))))
(take 10 expx)
(take 10 sinx)
(take 10 cosx)

; the following two should produces zeros
(take 10 (minus sinx (square-root (minus ps1 (sqr cosx)))))
(take 10 (minus (div sinx cosx) (revert (integral (div ps1 (plus ps1 (sqr psx)))))))

(def ts :stream (:: 1 (sqr ts))) 
(take 10 ts)  ; catalan numbers

(def tree   :stream (hold (:: 0 forest)))
(def forest :stream (hold (compose list1 tree)))
(def list1  :stream (hold (:: 1 list1)))
(take 10 tree) ; catalan numbers





