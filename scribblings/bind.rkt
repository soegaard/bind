#lang scribble/doc
@(require scribble/base
          scribble/manual
          (for-syntax racket/base racket/path)
          (for-label scribble/base))
@(require scribble/eval (for-label racket "../bind.rkt"))
@(define beval (make-base-eval))
@(beval '(require racket "../bind.rkt"))

@local-table-of-contents[#:style 'immediate-only]
@author[@author+email["Jens Axel Søgaard" "jensaxel@soegaard.net"]]

@defmodule[bind]

@section{Bind: Local bindings and binding clause transformers}

@subsection{Bind}

This manual documents the binding construct @racket[bind] and
various binding clause transformers.

The binding construct @racket[bind] is a generalization of @racket[let]
and @racket[let-values].

@specform/subs[
(bind (clause ...) body ...+)
([clause [id expr]
         [id bct-id expr]
         [(id ...) expr]         
         [(id ...) bct-id expr]])
]{}

Evaluates @racket[_body ...]. The clauses introduce bindings and 
syntax bindings whose scope includes @racket[body]. 

In the simple case, each @racket[_clause] has the form @racket[[_id _expr]].
In this simple case @racket[bind] is equivalent to @racket[let]. 

In the general case each binding clause has an associated binding clause
transformer. A binding clause transformer is a function that receives
a clause represented as a syntax object and returns two values: a list
of @racket[let-values] binding clauses and a list of @racket[let-syntax]
binding clauses. The bindings for all clauses are then spliced into 
the following:

@specform[(let-values ([(id ...) e] 
                        ...)
            (let-syntax ([(id ...) e]
                         ...)
              body ...))]

Example: 

Euclid is writting a library for plane geometry and given
a vector @racket[_v] want to use @racket[_v0] and 
@racket[_v1] to refer to the entries of the vector.
His solution is to define a binding clause transformer @racket[vec2].

@examples[#:eval beval
(require (for-syntax syntax/parse racket/syntax))
(define-binding-clause-transformer (:vec2 stx)
  (syntax-parse stx
    [(id:id _ expr)
     (with-syntax ([id0 (format-id #'id "~a0" #'id)]
                   [id1 (format-id #'id "~a1" #'id)])
       (values
        ; The let-values binding clauses:
        (list #'[(id id0 id1) 
                 (let ([v expr])
                   (values v
                           (vector-ref v 0)
                           (vector-ref v 1)))])
        ; The let-syntax binding clauses:
        (list)))]))

(bind ([v :vec2 #(3 4)])
      (displayln 
       (~a "The vector " v " has entries " v0 " and " v1)))]

@subsection{Binding Clause Transformers}

@specform[[id :delay e]]

The binding clause transformer @racket[:delay] 
binds @racket[_id] to the result of @racket[(delay e)].
Furthermore in body of the @racket[bind] expression,
the following bindings are in place: 
References to @racket[_id] expand to @racket[(force id)].
Applictions @racket[(id arg ...)] expand to @racket[((force id) arg ...)].

@examples[#:eval beval
                 (bind ([x :delay (/ 1 0)]) 3)
                 (bind ([x :delay 3]) (+ x x))
                 (bind ([x :delay (/ 1 0)]) (set! x 4) (+ x x))]

@specform[[(id ...) :same e]]
The binding clause
@racketblock[[(id ...) :same e]]
will evaluate the expression @racket[_e ]and bind the
result value to the identifiers #racket[_id ...].

@examples[#:eval beval
                 (bind ([(x y) :same 1]) (list x y))]

@specform[[id       :match pat e]]
@specform[[(id ...) :match pat e]]

The binding clause
@racketblock[[id :match pat e]]
will match the pattern @racket[pat] against the result 
of the expression @racket[e]. The identifier @racket[id] must 
appear in the pattern.

The binding clause
@racketblock[[(id ...) :match pat e]]
will match the pattern @racket[pat] against the result of the expression @racket[e].
All identifiers @racket[id ...] must appear in the pattern.

@examples[#:eval beval
                 (bind ([x :match (list x _) '(2 3)]) x)
                 (bind ([(x y) :match (list x y) '(2 3)]) (+ x y))
                 (bind ([x :match (list _ _) '(2 3)]) x)]


@specform[[(x y) :complex e]]

The binding clause
@racketblock[[(x y) :complex e]]
will evaluate the expression @racket[e] and bind the real part to
@racket[_x] and the imaginary part to @racket[_y] in the body of the 
bind expression.

@examples[#:eval beval
                 (bind ([(x y) :complex (sqrt -4)]) (list x y))]

@specform[[id :vector n e]]

The binding clause
@racketblock[[id :vector n e]]
will evaluate the expression @racket[_e] and bind the value to @racket[id].
Furthermore the @racket[_n] identifiers @racket[d0, id1, ...] 
will in the body of @racket[bind] expand to @racket[(vector-ref id i)].
Also @racket[(set! idi e)] will expand to @racket[(vector-set! v i e)].

@examples[#:eval beval
                 (bind ([v :vector 2 (vector 3 4)])
                       (set! v1 5)
                       (+ v0 v1))]
