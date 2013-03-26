    1 Bind: Local bindings and binding clause transformers

Jens Axel Søgaard
<[jensaxel@soegaard.net](mailto:jensaxel@soegaard.net)>

```racket
 (require bind)
```

# 1. Bind: Local bindings and binding clause transformers

## 1.1. Bind

This manual documents the binding construct `bind` and various binding
clause transformers.

The binding construct `bind` is a generalization of `let` and
`let-values`.

```racket
(bind (clause ...) body ...+)  
                               
clause = [id expr]             
       | [id bct-id expr]      
       | [(id ...) expr]       
       | [(id ...) bct-id expr]
```

Evaluates `body`` ``...`. The clauses introduce bindings and syntax
bindings whose scope includes `body`.

In the simple case, each `clause` has the form `[``id`` ``expr``]`. In
this simple case `bind` is equivalent to `let`.

In the general case each binding clause has an associated binding clause
transformer. A binding clause transformer is a function that receives a
clause represented as a syntax object and returns two values: a list of
`let-values` binding clauses and a list of `let-syntax` binding clauses.
The bindings for all clauses are then spliced into the following:

```racket                
(let-values ([(id ...) e]  
              ...)         
  (let-syntax ([(id ...) e]
               ...)        
    body ...))                    
```

Example:

Euclid is writting a library for plane geometry and given a vector `v`
want to use `v0` and `v1` to refer to the entries of the vector. His
solution is to define a binding clause transformer `vec2`.


Examples:                                                    
```racket                                                    
> (require (for-syntax syntax/parse racket/syntax))          
```                                     
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
                                                          
The vector #(3 4) has entries 3 and 4                        
```

## 1.2. Binding Clause Transformers

```racket
[id :delay e]
```

The binding clause transformer `:delay` binds `id` to the result of
`(``delay`` ``e``)`. Furthermore in body of the `bind` expression, the
following bindings are in place: References to `id` expand to
`(``force`` ``id``)`. Applictions `(``id`` ``arg`` ``...``)` expand to
`(``(``force`` ``id``)`` ``arg`` ``...``)`.


Examples:                                       
```racket                                       
> (bind ([x :delay (/ 1 0)]) 3)                 
3                                               
> (bind ([x :delay 3]) (+ x x))                 
6                                               
> (bind ([x :delay (/ 1 0)]) (set! x 4) (+ x x))
8                                               
```                                             


```racket
[(id ...) :same e]
```

The binding clause

`[``(``id` `...``)` `:same` `e``]`

will evaluate the expression `e`and bind the result value to the
identifiers \#racket[\_id ...].


Example:                             
```racket                            
> (bind ([(x y) :same 1]) (list x y))
'(1 1)                               
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

Examples:                                          
```racket                                          
> (bind ([x :match (list x _) '(2 3)]) x)          
2                                                  
> (bind ([(x y) :match (list x y) '(2 3)]) (+ x y))
5                                                  
> (bind ([x :match (list _ _) '(2 3)]) x)          
x: undefined;                                      
 cannot reference undefined identifier             
```

```racket
[(x y) :complex e]
```

The binding clause

`[``(``x` `y``)` `:complex` `e``]`

will evaluate the expression `e` and bind the real part to `x` and the
imaginary part to `y` in the body of the bind expression.

Example:                                        
```racket                                       
> (bind ([(x y) :complex (sqrt -4)]) (list x y))
'(0 2)                                          
```                                             


```racket
[id :vector n e]
```

The binding clause

`[``id` `:vector` `n` `e``]`

will evaluate the expression `e` and bind the value to `id`. Furthermore
the `n` identifiers `d0`` ``,``id1`` ``,``...` will in the body of
`bind` expand to `(``vector-ref`` ``id`` ``i``)`. Also `(``set!``
``idi`` ``e``)` will expand to `(``vector-set!`` ``v`` ``i`` ``e``)`.


Example:                                                     
```racket                           
> (bind ([v :vector 2 (vector 3 4)])
        (set! v1 5)                 
        (+ v0 v1))                                                  
8                                                                 
```
