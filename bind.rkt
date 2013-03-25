#lang racket
(provide #%bind-clause bind
         define-binding-clause-transformer
         (struct-out binding-clause-transformer)
         (for-syntax :delay :same :match :complex :vector))

(require "private/bind.rkt"
         "examples/delay.rkt"
         "examples/same.rkt"
         "examples/match.rkt"
         "examples/complex.rkt"
         "examples/vector.rkt")



