#lang racket
(provide (struct-out binding-clause-transformer))

(struct binding-clause-transformer 
    (name transformer))

(struct binding-clause-conventions
  (get-procedures get-rules) #:transparent)

  