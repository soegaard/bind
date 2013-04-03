#lang racket
(require "../private/bind.rkt"
         racket/syntax
         (for-syntax syntax/parse
                     racket/syntax))
(provide (for-syntax :string :string/idx :str))

; The following string functions matches Python:
;  http://docs.python.org/2/library/stdtypes.html
;  #sequence-types-str-unicode-list-tuple-bytearray-buffer-xrange

(define (string-min s)
  (if (= (string-length s) 0)
      (error 'string-min "the empty string has no minimum character")
      (for/fold ([m (string-ref s 0)])
        ([c (in-string s)])
        (if (char<? c m) c m))))

(define (string-max s)
  (if (= (string-length s) 0)
      (error 'string-max "the empty string has no maximum character")
      (for/fold ([m (string-ref s 0)])
        ([c (in-string s)])
        (if (char>? c m) c m))))

(define (string-count-char s c)
  (for/sum ([d (in-string s)]
            #:when (char=? c d))
    1))

(define (substring= s l r  t x y)
  (and (= (- r l) (- y x))
       (for/and ([i (in-range l r)] [j (in-range x y)])
         (char=? (string-ref s i) (string-ref t j)))))

(define (string-count-string s t [l 0] [r (string-length s)])
  ; Return the number of non-overlapping occurrences of substring t
  ; in the range [start, end]. Optional arguments start and end are 
  ; interpreted as in slice notation.
  (define n (string-length t))
  (for/sum ([l (in-range l (+ (- r n) 1))]
            #:when (substring= t 0 n  s l (+ l n)))
    1))

(define (string-count s x)
  (cond [(char? x)   (string-count-char s x)]
        [(string? x) (string-count-string s x)]
        [else (error)]))

(define (string-index s c)
  (for/first ([i (in-naturals)]
              [d (in-string s)]
              #:when (char=? c d))
    i))

(define (string-mult s n)
  (cond [(<= n 0) ""]
        [(= n 1) s]
        [else
         (apply string-append
                (for/list ([i (max n 0)]) s))]))

(define (string-ref* s i)
  ; Python style string-ref
  (if (negative? i)
      (string-ref s (+ (string-length s) i))
      (string-ref s i)))

(define (string-capitalize s)
  (string-append (string (char-upcase (string-ref s 0)))
                 (substring s 1 (string-length s))))

(define (string-center s width [fill #\space])
  ; Return centered in a string of length width.
  ; The character fill is used as padding.
  (let ([n (string-length s)])
    (define pad-size (max 0 (- width n)))
    (define left-pad (quotient pad-size 2))
    (define right-pad (- pad-size left-pad))
    (string-append (make-string left-pad fill)
                   s
                   (make-string right-pad fill))))

(define (string-ends-with? s t)
  ; does s end with t?
  (define m (string-length s))
  (define n (string-length t))
  (cond
    [(< m n) #f]
    [(= m n) (string=? s t)]
    [else    (for/and ([i (in-range (- m n))])
               (char=? (string-ref s (- m i 1))
                       (string-ref t (- n i 1))))]))

(define (string-expand-tabs s [tab-size 8])
  (define tabs (string-count-char s #\tab))
  (define n (string-length s))
  (define e (make-string (+ n (* tabs (- tab-size 1))) #\space))
  (define k 0)
  (for ([c (in-string s)] [i (in-naturals)])
    (cond [(char=? c #\tab) (set! k (+ k tab-size))]
          [else             (string-set! e k (string-ref s i))
                            (set! k (+ k 1))]))
  e)

(define (string-find s sub [l 0] [r (string-length s)])
  (set! l (max 0 l))
  (set! r (min r (string-length s)))
  (def n (string-length sub))
  (def l+n (+ l n))
  (and (< n (string-length s))
       (for/first ([i (in-range l (- r n -1))]
                   #:when (substring= s (+ l i) (+ l+n i)
                                      sub 0 n))
         i)))

(define-syntax (define-string-pred stx)
  (syntax-parse stx
    [(_ name?:id c:id e:expr)
     (syntax/loc stx
       (define (name? s)
         (def n (string-length s))
         (and (positive? n)
              (for/and ([c (in-string s)])
                e))))]))

(define-string-pred string-digit? c (char-numeric? c))
(define-string-pred string-lower? c (char-lower-case? c))
(define-string-pred string-upper? c (char-upper-case? c))
(define-string-pred string-space? c (char-whitespace? c))
(define-string-pred string-alpha? c (char-alphabetic? c))
(define-string-pred string-alnum? c 
  (or (char-alphabetic? c) (char-numeric? c)))

; is-title?



; Semantics:
;   The binding clause
;       [s :string e]
;   will evaluate the expression e and bind the result to s.
;   In the body of the bind expression:
;       (s i)     expands to (string-ref s i)
;       (s! i x)  expands to (string-set! s i x)
;        s!       expands to (λ (i x) (vector-set! s i x))

(define-binding-clause-transformer (:string stx)
  (syntax-parse stx
    [(v*:id _ e:expr)
     (define (make-id format-str) (format-id #'v* format-str #'v*))
     (with-syntax ([v             (generate-temporary #'v*)]
                   [v!            (make-id "~a!")]
                   [v.len         (make-id "~a.len")]
                   [in-v          (make-id "in-~a")]
                   [v+            (make-id "~a+")]
                   [v=            (make-id "~a=")]
                   [v**           (make-id "~a*")]
                   [v.index       (make-id "~a.index")]
                   [v.count       (make-id "~a.count")]
                   [v.min         (make-id "~a.min")]
                   [v.max         (make-id "~a.max")]
                   [v.capitalize  (make-id "~a.capitalize")]
                   [v.center      (make-id "~a.center")]
                   [v.ends-with?  (make-id "~a.ends-with?")]
                   [v.expand-tabs (make-id "~a.expand-tabs")]
                   [v.find        (make-id "~a.find")]
                   [v.alnum?      (make-id "~a.alnum?")]
                   [v.digit?      (make-id "~a.digit?")]
                   [v.lower?      (make-id "~a.lower?")]
                   [v.upper?      (make-id "~a.upper?")]
                   [v.space?      (make-id "~a.space?")]
                   [v.alpha?      (make-id "~a.alpha?")])
       (values
        (list #'[(v) e])
        (list #`[v* (make-transformer
                     #:context so #:literals (set!)
                     [(set! w e) (syntax/loc so (set! v i e))]
                     [(w k)      (syntax/loc so (string-ref* v k))]
                     [(w k l)    (syntax/loc so (substring v k l))]
                     [(w . more) (syntax/loc so (#%app v . more))]
                     [i:id       (syntax/loc so v)])]
              #`[v! (make-transformer
                     #:context so #:literals (set!)
                     [(w! . more) (syntax/loc so (string-set! v . more))]
                     [w!:id       (syntax/loc so (λ (i x) (string-set! v i x)))])]
              #`[v= (make-transformer
                     #:context so #:literals (set!)
                     [(_ . more) (syntax/loc so (string=? v . more))]
                     [_:id       (syntax/loc so (λ (x) (string=? v x)))])]
              #`[v.len (make-transformer
                        #:context so #:literals (set!)
                        [_:id (syntax/loc so (string-length v))])]
              #`[in-v (make-transformer
                       #:context so #:literals (set!)
                       [_:id (syntax/loc so (in-string v))])]
              #`[v+ (make-transformer
                     #:context so #:literals (set!)
                     [(_:id . more)
                      (syntax/loc so (string-append v . more))])]
              #`[v** (make-transformer
                     #:context so #:literals (set!)
                     [(_:id n:expr)
                      (syntax/loc so (string-mult v n))])]
              #`[v.index
                 (make-transformer
                  #:context so #:literals (set!)
                  [(_:id c) (syntax/loc so (string-index v c))])]
              #`[v.count
                 (make-transformer
                  #:context so #:literals (set!)
                  [(_:id c/s) (syntax/loc so (string-count v c/s))])]
              #`[v.min
                 (make-transformer
                  #:context so #:literals (set!)
                  [_:id (syntax/loc so (string-min v))])]
              #`[v.max
                 (make-transformer
                  #:context so #:literals (set!)
                  [_:id (syntax/loc so (string-max v))])]
              #`[v.capitalize
                 (make-transformer
                  #:context so #:literals (set!)
                  [_:id (syntax/loc so (string-capitalize v))])]
              #`[v.center
                 (make-transformer
                  #:context so #:literals (set!)
                  [(_:id . more) (syntax/loc so (string-center v . more))])]
              #`[v.ends-with?
                 (make-transformer
                  #:context so #:literals (set!)
                  [(_:id . more) (syntax/loc so (string-ends-with? v . more))])]
              #`[v.expand-tabs
                 (make-transformer
                  #:context so #:literals (set!)
                  [(_:id . more)  (syntax/loc so (string-expand-tabs v . more))])]
              #`[v.find
                 (make-transformer
                  #:context so #:literals (set!)
                  [(_:id . more)  (syntax/loc so (string-find v . more))])]
              #`[v.alnum?
                 (make-transformer
                  #:context so #:literals (set!)
                  [_:id  (syntax/loc so (string-alnum? v))])]
              #`[v.digit?
                 (make-transformer
                  #:context so #:literals (set!)
                  [_:id  (syntax/loc so (string-digit? v))])]
              #`[v.lower?
                 (make-transformer
                  #:context so #:literals (set!)
                  [_:id  (syntax/loc so (string-lower? v))])]
              #`[v.upper?
                 (make-transformer
                  #:context so #:literals (set!)
                  [_:id  (syntax/loc so (string-upper? v))])]
              #`[v.space?
                 (make-transformer
                  #:context so #:literals (set!)
                  [_:id  (syntax/loc so (string-space? v))])]
              #`[v.alpha?
                 (make-transformer
                  #:context so #:literals (set!)
                  [_:id  (syntax/loc so (string-alpha? v))])])))]))

(define-for-syntax :str :string)

; Semantics:
;   The binding clause
;       [s :string/idx n e]
;   will evaluate the expression e and bind the result to v.
;   In the body of the bind expression the n identifiers
;        s0, s1, ...
;   will expand to (string-ref s i).
;   Furthermore (set! si e) will expand to (string-set! s i e).

(define-binding-clause-transformer (:string/idx stx)
  (syntax-parse stx
    [(id:id _ n:exact-nonnegative-integer expr)
     (let ([n (syntax-e #'n)])
       (with-syntax ([(idi ...)
                      (for/list ([i n])
                        (format-id #'id "~a~a" #'id i))]
                     [(j ...) (for/list ([j n]) j)])
         (values
          (list #'[(id*) expr])
          (append
           (list #`[id (make-transformer
                        #:context so
                        #:literals (set!)
                        [(set! i e)   (syntax/loc so (set! id* i e))]
                        [(i k)        (syntax/loc so (string-ref id* k))]
                        [(i . more)   (syntax/loc so (#%app id* . more))]
                        [i:identifier (syntax/loc so id*)])])
           (map (λ (idi j)
                  (with-syntax ([idi idi] [j j])
                    #`[idi (make-transformer
                            #:context so
                            #:literals (set!)
                            [(set! i e)   (syntax/loc so (string-set! id* j e))]
                            [(i . more)   (syntax/loc so (#%app idi . more))]
                            [i:identifier (syntax/loc so (string-ref id* j))])]))
                (syntax->list #'(idi ...))
                (syntax->list #'(j ...)))))))]))

(module* test #f 
  (require rackunit)
  ; Note: It would be ideal for the match api to generate the
  ;       list of variables to be bound. The first example would be:
  ;       (bind ([_ :match (list x _) '(2 3)]) x)
  (check-equal? (bind ([s :string (string #\a #\b)])
                  (s! 0 #\c) 
                  (string (s 0) (s 1)))
                "cb")
  ; :vector/idx is cute, but is it useful?
  (check-equal? (bind ([s :string/idx 2 (string #\a #\b)])
                  (set! s1 #\c)
                  (string s0 s1))
                "ac")
  (bind ([s :string "foobar"]
         [f :string "foo"]
         [ff :string "foofoo"]
         [t  :string "foo\tbar\t\newline"])
    (check-equal? (s 1) #\o)
    (check-equal? (s -1) #\r)
    (check-equal? (s -2) #\a)
    (check-equal? (s 1 s.len) "oobar")
    (check-equal? (s 1 (sub1 s.len)) "ooba")
    (check-equal? (s+ "bar") "foobarbar")
    (check-equal? (f+ "bar") "foobar")
    (check-equal? (f* -1) "")
    (check-equal? (f* 0) "")
    (check-equal? (f* 1) "foo")
    (check-equal? (f* 2) "foofoo")
    (check-equal? (s.index #\a) 4)
    (check-equal? (s.index #\x) #f)
    (check-equal? (s.count #\o) 2)
    (check-equal? (s.count #\f) 1)
    (check-equal? (s.count #\x) 0)
    (check-equal? (s.count "o") 2)
    (check-equal? (s.count "f") 1)
    (check-equal? (ff.count "foo") 2)
    (check-equal? s.min #\a)
    (check-equal? s.max #\r)
    (check-equal? s.capitalize "Foobar")
    (check-equal? (f.center 5) " foo ")
    (check-equal? (f.center 7 #\x) "xxfooxx")
    (check-equal? (f.center 6 #\x) "xfooxx")
    (check-equal? (s.ends-with? "bar") #t)
    (check-equal? (s.ends-with? "baz") #f)
    (check-equal? (s.expand-tabs) s)
    (check-equal? (t.expand-tabs) 
                  (string-replace t "\t" "        "))
    (check-equal? (s.find "oob") 1)
    (check-equal? (s.find "oobart") #f)
    (check-equal? (s.find "foobart") #f)
    (check-equal? (s.find "ar") 4)
    (check-equal? s.alnum? #t)
    (check-equal? t.alnum? #f)
    (check-equal? (bind ([s :str ""]) s.alnum?) #f)
    (check-equal? (bind ([s :str "ab34b34nb34"]) s.alnum?) #t)
    (check-equal? (bind ([s :str "ab34b34nb!34"]) s.alnum?) #f)
    (check-equal? (bind ([s :str "123"]) s.digit?) #t)
    (check-equal? (bind ([s :str "12x3"]) s.digit?) #f)
    (check-equal? (bind ([s :str ""]) s.digit?) #f)
    (check-equal? (bind ([s :str "ABC"]) s.upper?) #t)
    (check-equal? (bind ([s :str "abC"]) s.upper?) #f)
    (check-equal? (bind ([s :str ""]) s.upper?) #f)
    (check-equal? (bind ([s :str "abc"]) s.lower?) #t)
    (check-equal? (bind ([s :str "abC"]) s.lower?) #f)
    (check-equal? (bind ([s :str "Abc"]) s.alpha?) #t)
    (check-equal? (bind ([s :str "abC3"]) s.alpha?) #f)
    (check-equal? (bind ([s :str " \t\n "]) s.space?) #t)
    (check-equal? (bind ([s :str " a  "]) s.space?) #f)
    (check-equal? (for/list ([c in-s]) c) (string->list s))
    (check-equal? (bind ([s :str "foo"]) (s= "bar")) #f)
    (check-equal? (bind ([s :str "foo"]) (s= "foo")) #t)))

