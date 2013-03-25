#lang racket
(provide (struct-out binding-clause-transformer))

(struct binding-clause-transformer 
    (name transformer))
