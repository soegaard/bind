#lang scribble/manual
@(require scribble/base
          scribble/manual
          (for-syntax racket/base racket/path)
          (for-label scribble/base)
          "../bind.rkt"
          (for-label racket bind))
@(require scribble/eval (for-label racket bind))
@(define beval (make-base-eval))
@(beval '(require racket bind))

@local-table-of-contents[#:style 'immediate-only]

@title{Bind: One construct to bind them all}
@author[@author+email["Jens Axel Søgaard" "jensaxel@soegaard.net"]]

@defmodule[bind]

@section{Bind: A Local binding construct}

@subsection{Informal Introduction}

This manual documents the binding construct @racket[bind] and
various binding clause transformers. What makes @racket[bind]
interesting is not the support of multiple values, but rather
that each binding clause can introduce multiple bindings
both value and syntax bindings. 

The purpose of @racket[let-values] and @racket[let-syntax]
is to setup a scope in which to evaluate an expression.
For some expressions both are needed:
@racketblock[(let-values (clause ...) 
               (let-syntax (clause ...)
                 expr))]

Let us consider the example, where we have a delayed expression:
@racketblock[(let ([x (delay 1)])
               (+ (force x) (force x)))]
It is a bit cumbersome to write @racket[(force x)] each time
the value of the delayed expression is needed. Therefore one
can introduce a little syntax to makes things easier:
@racketblock[(let ([x (delay 1)])
               (let-syntax ([clause ...])
                 (+ x x)))]
where @racket[_clause] makes @racket[_x] expand to @racket[(force x)].
Using @racket[bind] one can define a binding clause transformer
@racket[:delay] and then simply write:
@racketblock[(bind ([x :delay 1]
               (+ x x)))]
During expansion of @racket[bind] the binding clause transformer 
@racket[_:delay] will be called with the clause @racket[#'[x :delay 1]]
as input. It will return two lists:  a list of @racket[let-values] clauses
and a list of @racket[let-syntax] clauses. The expander will then
splice these clauses together into:
@racketblock[(let ([x (delay 1)])
               (let-syntax ([clause ...])
                 (+ x x)))]

To summarize: @racket[bind] can be used as a dropin replacement 
of @racket[let] and @racket[let-values]. Using binding clause transformers
a single binding clause can expand into multiple @racket[let-values]
and @racket[let-syntax] clauses. A few examples are in order
before the more formal specification:

@examples[#:eval beval
; First a class definition:                 
(define fish%
  (class object% (init size)                
    (super-new)
    (define current-size size)
    (define/public (get-size) current-size)    
    (define/public (grow amt) (set! current-size (+ amt current-size)))))
; Now :object allows to use objects with an implicit send
(bind ([charlie :object (new fish% [size 10])])
  (charlie grow 5)
  (charlie get-size))]

@examples[#:eval beval
(bind ([v :vector (for/vector ([i 5]) (random 10))])
  (displayln (~a "The vector " v " contains " v))
  (displayln (~a "The first element is: " (v 0)))
  (v! 0 42)
  (displayln (~a "The first element is now: " (v 0)))
  (displayln (~a "The middle three elements are: " (v 1 4))))]


@subsection{Bind}

@defform/subs[
(bind (clause ...) body ...+)
([clause [id expr]
         [(id ...) expr]         
         [id bct expr]
         [(id ...) bct expr]])
]{}

Evaluates @racket[_body ...]. The @racket[_clause]s introduce bindings and 
syntax bindings whose scope includes @racket[_body]. 

In the simple case, each @racket[_clause] has the form @racket[[_id _expr]].
In this simple case @racket[bind] is equivalent to @racket[let]. 

In the general case each binding clause has an associated binding clause
transformer. A binding clause transformer is a function that receives
a clause represented as a syntax object and returns two values: a list
of @racket[let-values] binding clauses and a list of @racket[let-syntax]
binding clauses. The bindings for all clauses are then spliced into 
the following:

@racketblock[(let-values ([(id ...) e] ...)
               (let-syntax ([(id ...) e] ...)
                 body ...))]

The binding clauses with an explicit binding clause transformer (bct)
are @racket[[id bct expr]] and @racket[[(id ...) bct expr]]. Here
@racket[_bct] must be bound to a binding clause transformer otherwise
an error is signaled. 

The binding clauses without an explicit binding clause transformer,
namely @racket[[id expr]] and @racket[[(id ...) expr]] will be
expanded by the default binding clause transformer @racket[#%bind-clause].

This manual describes the builtin binding clause transformer, but
see @racket[define-binding-clause-transformer] on how to define
your own.

@defform*[((def id e)
           (def (id ...) e)
           (def id bct e ...+)
           (def (id ...) bct e ...+))]

Note: @racket[def] is not meant to work as a dropin for @racket[define].

In the simple cases @racket[(def id e)] the expression @racket[e]
is evaluated and the result is bound to @racket[_id].

In the case @racket[(def (id ...) e)] the expression @racket[e]
is evaluated. It must return the same number of values as there
are identifiers @racket[id ...]. Each value is bound to an identifier.

In the general case @racket[(def id bct e ...)] the binding clause
transformer @racket[_bct] is called with the binding clause 
@racket[[id bct e ...]] as input. The resulting @racket[let-values]
clauses and @racket[let-syntax] clauses are spliced into
a @racket[define-values] and @racket[define-syntax] definitions.
@racketblock[(define-values (id ...) e) ...
             (define-syntax (id ...) e) ...]

@examples[#:eval beval
                 (def v :vector #(3 4))
                 (v 0)]


@subsection{Binding Clause Transformers}

@defform[#:kind "bct" #:id :delay
                [id :delay e]]

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

@;--------------------

@defform[#:kind "bct" #:id :object
                [id :object e]]

The binding clause transformer @racket[:object] 
binds @racket[_id] to the result of the expression @racket[(delay e)].
In the body of @racket[bind] applications of the form 
@racketblock[(id arg ...)]
will expand to
@racketblock[(send id arg ...)]

@examples[#:eval beval
(define fish%
  (class object% (init size)                
    (super-new)
    (define current-size size)
    (define/public (get-size) current-size)    
    (define/public (grow amt) (set! current-size (+ amt current-size)))))
; Now :object allows to use objects with an implicit send
(bind ([charlie :object (new fish% [size 10])])
  (charlie grow 5)
  (charlie get-size))]

@;--------------------

@defform[#:kind "bct" #:id :vector
                [id :vector e]]

The binding clause
@racketblock[[id :vector e]]
will evaluate the expression @racket[_e] and bind the result to @racket[id].
In the body of the bind expression:

@racket[(v i)]     expands to @racket[(vector-ref v i)]

@racket[(v i j)]   expands to @racket[(vector-copy v i j)]

@racket[(v! i x)]  expands to @racket[(vector-set! v i x)]

@racket[v!]       expands to  @racket[(λ (i x) (vector-set! v i x))]


@examples[#:eval beval
                 (bind ([v :vector (vector 3 4)])
                       (v! 1 5)
                       (+ (v 0) (v 1)))]

@;--------------------

@defform[#:kind "bct" #:id :string
                [id :string e]]

The binding clause
@racketblock[[id :string e]]
will evaluate the expression @racket[_e] and bind the result to @racket[id].
In the body of the bind expression:

@racket[(id i)]     expands to @racket[(string-ref id i)]

@racket[(id i j)]   expands to @racket[(substring id i j)]

@racket[(id! i x)]  expands to @racket[(string-set! id i x)]

@racket[id!]       expands to  @racket[(λ (i x) (string-set! id i x))]

@examples[#:eval beval
                 (bind ([s :string "foobar"])
                   (~a (s 3 6) (s 0) (s 1) (s 2)))]


@;--------------------


@defform[#:kind "bct" #:id :same
                [(id ...) :same e]]
The binding clause
@racketblock[[(id ...) :same e]]
will evaluate the expression @racket[_e ]and bind the
result value to the identifiers #racket[_id ...].

@examples[#:eval beval
                 (bind ([(x y) :same 1]) (list x y))]

@defform[#:kind "bct" #:id :match
                [id       :match pat e]]
@defform[#:kind "bct" #:id :match
                [(id ...) :match pat e]]

@;--------------------

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
                 (bind ([(x y) :match (list x y) '(2 3)]) (+ x y))]

@;--------------------


@defform[#:kind "bct" #:id :complex
                [(x y) :complex e]]

The binding clause
@racketblock[[(x y) :complex e]]
will evaluate the expression @racket[e] and bind the real part to
@racket[_x] and the imaginary part to @racket[_y] in the body of the 
bind expression.

@examples[#:eval beval
                 (bind ([(x y) :complex (sqrt -4)]) (list x y))]

@;--------------------


@defform[#:kind "bct" #:id :vector/idx
         [id :vector/idx n e]]

The binding clause
@racketblock[[id :vector/idx n e]]
will evaluate the expression @racket[_e] and bind the result to @racket[id].
Furthermore the @racket[_n] identifiers @racket[d0, id1, ...] 
will in the body of @racket[bind] expand to @racket[(vector-ref id i)].
Also @racket[(set! idi e)] will expand to @racket[(vector-set! v i e)].

@examples[#:eval beval
                 (bind ([v :vector/idx 2 (vector 3 4)])
                       (set! v1 5)
                       (+ v0 v1))]

@subsection{Defining binding clause transformers}

@defform*[((define-binding-clause-transformer id expr)
           (define-binding-clause-transformer (id arg) expr))]

Use @racket[define-binding-clause-transformer] to define
binding clause transformers. The form 
@racket[define-binding-clause-transformer] works as 
@racket[define-syntax] except it binds a binding
clause transformer and not a syntax transformer.

The input of a binding clause transformer is a syntax
object representing the binding clause. The output
is two values: the first is a list of @racket[let-values]
binding clauses, the second is a list of @racket[let-syntax]
binding clauses.

Let us consider an example:

Euclid is writting a library for plane geometry and given
a vector @racket[_v], he wants to use @racket[_v0] and 
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

@subsection{Assignment transformers}

@defform[(make-transformer
          #:context id
          #:literals (id ...)
          [pat expr] ...)]

The standard construct @racket[make-set!-transformer] used
to create assignment transformers is a little cumbersome
to use. The form @racket[make-transformer] is intended
to do the same job, with a slightly easier syntax.

Let us see how it is used to define the @racket[:delay]
binding clause transformer.

@racketblock[
(define-binding-clause-transformer (:delay stx)
  (syntax-parse stx
    [(id:id _ expr)
     (values
      (list #'[(id1) (delay expr)])
      (list 
       #`[id (make-transformer
              #:context so
              #:literals (set!)
              [(set! i e)   (syntax/loc so (set! id1 e))]
              [(i . more)   (syntax/loc so (#%app (force id1) . more))]
              [i:identifier (syntax/loc so (force id1))])]))]))
]


