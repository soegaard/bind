Bind: One construct to bind them all

    1 Bind: A Local binding construct

Jens Axel Søgaard
<[jensaxel@soegaard.net](mailto:jensaxel@soegaard.net)>

```racket
 (require bind)
```

# 1. Bind: A Local binding construct

## 1.1. Informal Introduction

This manual documents the binding construct `bind` and various binding
clause transformers. What makes `bind` interesting is not the support of
multiple values, but rather that each binding clause can introduce
multiple bindings both value and syntax bindings.

The purpose of `let-values` and `let-syntax` is to setup a scope in
which to evaluate an expression. For some expressions both are needed:

```racket
(let-values (clause ...)  
  (let-syntax (clause ...)
    expr))                
```

Let us consider the example, where we have a delayed expression:

```racket
(let ([x (delay 1)])      
  (+ (force x) (force x)))
```

It is a bit cumbersome to write `(``force`` ``x``)` each time the value
of the delayed expression is needed. Therefore one can introduce a
little syntax to makes things easier:

```racket
(let ([x (delay 1)])        
  (let-syntax ([clause ...])
    (+ x x)))               
```

where `clause` makes `x` expand to `(``force`` ``x``)`. Using `bind` one
can define a binding clause transformer `:delay` and then simply write:

```racket
(bind ([x :delay 1]
  (+ x x)))        
```

During expansion of `bind` the binding clause transformer `:delay` will
be called with the clause `#'``[``x`` ``:delay`` ``1``]` as input. It
will return two lists:  a list of `let-values` clauses and a list of
`let-syntax` clauses. The expander will then splice these clauses
together into:

```racket
(let ([x (delay 1)])        
  (let-syntax ([clause ...])
    (+ x x)))               
```

To summarize: `bind` can be used as a dropin replacement of `let` and
`let-values`. Using binding clause transformers a single binding clause
can expand into multiple `let-values` and `let-syntax` clauses. A few
examples are in order before the more formal specification:

```racket
Examples:                                                                  
```racket                                                                  
```racket                                                                  
> (define fish%                                                            
    (class object% (init size)                                             
      (super-new)                                                          
      (define current-size size)                                           
      (define/public (get-size) current-size)                              
      (define/public (grow amt) (set! current-size (+ amt current-size)))))
```                                                                        
                                                                           
```racket                                                                  
> (bind ([charlie :object (new fish% [size 10])])                          
    (charlie grow 5)                                                       
    (charlie get-size))                                                    
```                                                                        
15                                                                         
```                                                                        
```

```racket
Example:                                                       
```racket                                                      
```racket                                                      
> (bind ([v :vector (for/vector ([i 5]) (random 10))])         
    (displayln (~a "The vector " v " contains " v))            
    (displayln (~a "The first element is: " (v 0)))            
    (v! 0 42)                                                  
    (displayln (~a "The first element is now: " (v 0)))        
    (displayln (~a "The middle three elements are: " (v 1 4))))
```                                                            
The vector #(2 7 0 1 7) contains #(2 7 0 1 7)                  
The first element is: 2                                        
The first element is now: 42                                   
The middle three elements are: #(7 0 1)                        
                                                               
```                                                            
```

## 1.2. Bind

```racket
(bind (clause ...) body ...+)
                             
clause = [id expr]           
       | [(id ...) expr]     
       | [id bct expr]       
       | [(id ...) bct expr] 
```

Evaluates `body`` ``...`. The `clause`s introduce bindings and syntax
bindings whose scope includes `body`.

In the simple case, each `clause` has the form `[``id`` ``expr``]`. In
this simple case `bind` is equivalent to `let`.

In the general case each binding clause has an associated binding clause
transformer. A binding clause transformer is a function that receives a
clause represented as a syntax object and returns two values: a list of
`let-values` binding clauses and a list of `let-syntax` binding clauses.
The bindings for all clauses are then spliced into the following:

```racket
(let-values ([(id ...) e] ...)  
  (let-syntax ([(id ...) e] ...)
    body ...))                  
```

The binding clauses with an explicit binding clause transformer (bct)
are `[``id`` ``bct`` ``expr``]` and `[``(``id`` ``...``)`` ``bct``
``expr``]`. Here `bct` must be bound to a binding clause transformer
otherwise an error is signaled.

The binding clauses without an explicit binding clause transformer,
namely `[``id`` ``expr``]` and `[``(``id`` ``...``)`` ``expr``]` will be
expanded by the default binding clause transformer `#%bind-clause`.

This manual describes the builtin binding clause transformer, but see
`define-binding-clause-transformer` on how to define your own.

## 1.3. Binding Clause Transformers

```racket
[id :delay e]
```

The binding clause transformer `:delay` binds `id` to the result of
`(``delay`` ``e``)`. Furthermore in body of the `bind` expression, the
following bindings are in place: References to `id` expand to
`(``force`` ``id``)`. Applictions `(``id`` ``arg`` ``...``)` expand to
`(``(``force`` ``id``)`` ``arg`` ``...``)`.

```racket
Examples:                                       
```racket                                       
> (bind ([x :delay (/ 1 0)]) 3)                 
3                                               
> (bind ([x :delay 3]) (+ x x))                 
6                                               
> (bind ([x :delay (/ 1 0)]) (set! x 4) (+ x x))
8                                               
```                                             
```

```racket
[id :object e]
```

The binding clause transformer `:object` binds `id` to the result of the
expression `(``delay`` ``e``)`. In the body of `bind` applications of
the form

`(``id` `arg` `...``)`

will expand to

`(``send` `id` `arg` `...``)`

```racket
Examples:                                                                  
```racket                                                                  
```racket                                                                  
> (define fish%                                                            
    (class object% (init size)                                             
      (super-new)                                                          
      (define current-size size)                                           
      (define/public (get-size) current-size)                              
      (define/public (grow amt) (set! current-size (+ amt current-size)))))
```                                                                        
                                                                           
```racket                                                                  
> (bind ([charlie :object (new fish% [size 10])])                          
    (charlie grow 5)                                                       
    (charlie get-size))                                                    
```                                                                        
15                                                                         
```                                                                        
```

```racket
[id :vector e]
```

The binding clause

`[``id` `:vector` `e``]`

will evaluate the expression `e` and bind the result to `id`. In the
body of the bind expression:

`(``v`` ``i``)`     expands to `(``vector-ref`` ``v`` ``i``)`

`(``v`` ``i`` ``j``)`   expands to `(``vector-copy`` ``v`` ``i`` ``j``)`

`(``v!`` ``i`` ``x``)`  expands to `(``vector-set!`` ``v`` ``i`` ``x``)`

`v!`       expands to  `(``λ`` ``(``i`` ``x``)`` ``(``vector-set!``
``v`` ``i`` ``x``)``)`

```racket
Example:                          
```racket                         
```racket                         
> (bind ([v :vector (vector 3 4)])
        (v! 1 5)                  
        (+ (v 0) (v 1)))          
```                               
8                                 
```                               
```

```racket
[id :string e]
```

The binding clause

`[``id` `:string` `e``]`

will evaluate the expression `e` and bind the result to `id`. In the
body of the bind expression:

`(``id`` ``i``)`     expands to `(``string-ref`` ``id`` ``i``)`

`(``id`` ``i`` ``j``)`   expands to `(``substring`` ``id`` ``i`` ``j``)`

`(``id!`` ``i`` ``x``)`  expands to `(``string-set!`` ``id`` ``i``
``x``)`

`id!`       expands to  `(``λ`` ``(``i`` ``x``)`` ``(``string-set!``
``id`` ``i`` ``x``)``)`

```racket
Example:                           
```racket                          
```racket                          
> (bind ([s :string "foobar"])     
    (~a (s 3 6) (s 0) (s 1) (s 2)))
```                                
"barfoo"                           
```                                
```

```racket
[(id ...) :same e]
```

The binding clause

`[``(``id` `...``)` `:same` `e``]`

will evaluate the expression `e`and bind the result value to the
identifiers \#racket[\_id ...].

```racket
Example:                             
```racket                            
> (bind ([(x y) :same 1]) (list x y))
'(1 1)                               
```                                  
```

```racket
[id       :match pat e]
```

```racket
[(id ...) :match pat e]
```

The binding clause

`[``id` `:match` `pat` `e``]`

will match the pattern `pat` against the result of the expression `e`.
The identifier `id` must appear in the pattern.

The binding clause

`[``(``id` `...``)` `:match` `pat` `e``]`

will match the pattern `pat` against the result of the expression `e`.
All identifiers `id`` ``...` must appear in the pattern.

```racket
Examples:                                          
```racket                                          
> (bind ([x :match (list x _) '(2 3)]) x)          
2                                                  
> (bind ([(x y) :match (list x y) '(2 3)]) (+ x y))
5                                                  
```                                                
```

```racket
[(x y) :complex e]
```

The binding clause

`[``(``x` `y``)` `:complex` `e``]`

will evaluate the expression `e` and bind the real part to `x` and the
imaginary part to `y` in the body of the bind expression.

```racket
Example:                                        
```racket                                       
> (bind ([(x y) :complex (sqrt -4)]) (list x y))
'(0 2)                                          
```                                             
```

```racket
[id :vector/idx n e]
```

The binding clause

`[``id` `:vector/idx` `n` `e``]`

will evaluate the expression `e` and bind the result to `id`.
Furthermore the `n` identifiers `d0`` ``,``id1`` ``,``...` will in the
body of `bind` expand to `(``vector-ref`` ``id`` ``i``)`. Also
`(``set!`` ``idi`` ``e``)` will expand to `(``vector-set!`` ``v`` ``i``
``e``)`.

```racket
Example:                                
```racket                               
```racket                               
> (bind ([v :vector/idx 2 (vector 3 4)])
        (set! v1 5)                     
        (+ v0 v1))                      
```                                     
8                                       
```                                     
```

## 1.4. Defining binding clause transformers

```racket
(define-binding-clause-transformer id expr)      
(define-binding-clause-transformer (id arg) expr)
```

Use `define-binding-clause-transformer` to define binding clause
transformers. The form `define-binding-clause-transformer` works as
`define-syntax` except it binds a binding clause transformer and not a
syntax transformer.

The input of a binding clause transformer is a syntax object
representing the binding clause. The output is two values: the first is
a list of `let-values` binding clauses, the second is a list of
`let-syntax` binding clauses.

Let us consider an example:

Euclid is writting a library for plane geometry and given a vector `v`,
he wants to use `v0` and `v1` to refer to the entries of the vector. His
solution is to define a binding clause transformer `vec2`.

```racket
Examples:                                                    
```racket                                                    
> (require (for-syntax syntax/parse racket/syntax))          
                                                             
```racket                                                    
> (define-binding-clause-transformer (:vec2 stx)             
    (syntax-parse stx                                        
      [(id:id _ expr)                                        
       (with-syntax ([id0 (format-id #'id "~a0" #'id)]       
                     [id1 (format-id #'id "~a1" #'id)])      
         (values                                             
                                                             
          (list #'[(id id0 id1)                              
                   (let ([v expr])                           
                     (values v                               
                             (vector-ref v 0)                
                             (vector-ref v 1)))])            
                                                             
          (list)))]))                                        
```                                                          
                                                             
```racket                                                    
> (bind ([v :vec2 #(3 4)])                                   
        (displayln                                           
         (~a "The vector " v " has entries " v0 " and " v1)))
```                                                          
The vector #(3 4) has entries 3 and 4                        
                                                             
```                                                          
```

## 1.5. Assignment transformers

```racket
```racket           
(make-transformer   
 #:context id       
 #:literals (id ...)
 [pat expr] ...)    
```                 
```

The standard construct `make-set!-transformer` used to create assignment
transformers is a little cumbersome to use. The form `make-transformer`
is intended to do the same job, with a slightly easier syntax.

Let us see how it is used to define the `:delay` binding clause
transformer.

```racket
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
```
