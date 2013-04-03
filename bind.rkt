#lang racket
(provide #%bind-clause bind def with λ*
         define-binding-clause-transformer
         (for-syntax make-transformer)
         (struct-out binding-clause-transformer)
         (for-syntax           
          :complex
          :delay 
          :match
          :object
          :same
          :string :str :string/idx 
          :vector :vector/idx
          :integer :int
          :hash))

(require "private/bind.rkt"
         "examples/complex.rkt"
         "examples/delay.rkt"
         "examples/hash.rkt"
         "examples/integer.rkt"
         "examples/match.rkt"         
         "examples/object.rkt"
         "examples/vector.rkt"
         "examples/same.rkt"
         "examples/string.rkt")
