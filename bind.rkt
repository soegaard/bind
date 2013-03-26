#lang racket
(provide #%bind-clause bind def
         define-binding-clause-transformer
         (for-syntax make-transformer)
         (struct-out binding-clause-transformer)
         (for-syntax 
          :complex
          :delay 
          :match
          :object
          :same
          :string :string/idx
          :vector :vector/idx))

(require "private/bind.rkt"
         "examples/complex.rkt"
         "examples/delay.rkt"         
         "examples/match.rkt"         
         "examples/object.rkt"
         "examples/vector.rkt"
         "examples/same.rkt"
         "examples/string.rkt")

(define fish%
  (class object%
    (init size)                
    (define current-size size) 
    (super-new)                
    (define/public (get-size) current-size)    
    (define/public (grow amt) (set! current-size (+ amt current-size)))))

