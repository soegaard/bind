#lang racket
(require bind)
(module+ test (require rackunit))

; See tests in examples/string.rkt for how
; to use the Python inspired string operations.

; some utilities
(define-syntax in-N (syntax-id-rules (in-N) [in-N (in-naturals)]))
(define-syntax-rule (while expr body ...)
  (let loop () (when expr body ... (loop))))

; string-split : char string -> (listof string)
(define (string-split delimiter text)
  (with ([text :str])
    (def len :int text.len)
    (def l :int 0)
    (for/list ([r len]
               #:when (or (char=? (text r) delimiter)
                          (= r len-1)))
      (if (= r len-1)
          (text l len)
          (begin0 
            (text l r)
            (l! (+ r 1)))))))

(define (expand-tabs s [size 8])
  (with ([s :str] [size :int])
    (def tabs (s.count #\tab))
    (def new-len (+ s.len (* tabs size-1)))
  ; (def new-len {s.len+tabs*(size-1)}) ; with infix {}
    (def e :str (make-string new-len #\space))
    (def k :int 0)
    (for ([c in-s] [i in-N])
      (cond [(char=? c #\tab) (k+! size)]
            [else             (e! k (s i))
                              k++]))
    e))

(module+ test  
  (check-equal? (string-split #\, "foo,bar,baz")
                '("foo" "bar" "baz"))
  (check-equal? (string-split #\, "foo,,bar,baz")
                '("foo" "" "bar" "baz")))

; match-indices : string string -> (listof index)
;   return list of all indices i, such that
;      text[i..i+m] = pat
;   where m is the length of pat.
(define (match-indices pat text)
  ; naive algorithm 
  (with ([pat text :string])
    (def m pat.len)
    (def n text.len)
    (for/list ([i (- n m)]
               #:when (equal? pat (text i (+ i m))))
      i)))

(match-indices "foo" "Only a fool uses foo as food.")

(define (match-indices/old pat text)
  (define m (string-length pat))
  (define n (string-length text))
  (for/list ([i (- n m)]
             #:when (equal? pat (substring text i (+ i m))))
    i))

;algorithm kmp_search:
;    input:
;        an array of characters, S (the text to be searched)
;        an array of characters, W (the word sought)
;    output:
;        an integer (the zero-based position in S at which W is found)
;
;    define variables:
;        an integer, m ← 0 (the beginning of the current match in S)
;        an integer, i ← 0 (the position of the current character in W)
;        an array of integers, T (the table, computed elsewhere)
;
;    while m+i is less than the length of S, do:
;        if W[i] = S[m + i],
;            if i equals the (length of W)-1,
;                return m
;            let i ← i + 1
;        otherwise,
;            let m ← m + i - T[i],
;            if T[i] is greater than -1,
;                let i ← T[i]
;            else
;                let i ← 0
;            
;    (if we reach here, we have searched all of S unsuccessfully)
;    return the length of S
    

(define (kmp-search s w)
  ; input:  an array of characters, S (the text to be searched)
  ;         an array of characters, W (the word sought)
  ; output: an integer (the zero-based position in S at which W is found)
  (with ([s w :str])
    (def m :int 0) ; the beginning of the current match in S
    (def i :int 0) ; the position of the current character in W
    (def t :vector (kmp-table s)) ; the table, computed elsewhere)
    (let loop ()
      (cond [(>= (+ m i) s.len) #f]
            [(char=? (w i) (s (+ m i)))
             (cond [(= i+1 w.len) m]
                   [else i++ (loop)])]
            [else (m+! (- i (t i)))
                  (i! (max (t i) 0))
                  (loop)]))))

(define (kmp-table w)
  ; input:  an array of characters, W (the word to be analyzed)
  ;         an array of integers, T (the table to be filled)
  ; output: nothing (but during operation, it populates the table)
  (with ([w :str])
    (def t :vector (make-vector w.len 0))
    (def pos :int 2) ; the current position we are computing in T
    (def cnd :int 0) ; the zero-based index in W of the next 
                     ; character of the current candidate substring
    (t! 0 -1) (t! 1 0)
    (while (< pos w.len)
      (cond [(char=? (w pos-1) (w cnd)) cnd++ (t! pos cnd) pos++]
            [(> cnd 0)                  (cnd! (t cnd))]
            [else                       (t! pos 0) pos++]))
    t))


(define (substring= s l r  t x y)
  (with ([s t :str])
    (and (= (- r l) (- y x))
         (for/and ([i (in-range l r)] [j (in-range x y)])
           (char=? (s i) (t j))))))


; function RabinKarp(string s[1..n], string sub[1..m])
;     hsub := hash(sub[1..m]);  
;     hs := hash(s[1..m])
;     for i from 1 to n-m+1
;         if hs = hsub
;             if s[i..i+m-1] = sub
;                 return i
;         hs := hash(s[i+1..i+m])
;     return not found

; this hash function is a place holder
; replace with rolling hash
(def hash equal-hash-code)

(define (rabin-karp s t)
  ; search for t in s
  (with ([s t :str])
    (def n :int s.len)
    (def m :int t.len)
    (def hs :int 0)
    (def ht :int (hash (t 0 m)))
    (def found #f)
    (for ([i (in-range (- n+1 m))] #:break found)
      (hs! (hash (s i (+ i m)))) ; problem: allocates
      (when (and (= hs ht) (substring= s i (+ i m)  t 0 m))
        (set! found i)))
    found))

(define (string-prefixes s)
  (with ([s :str])
    (for/list ([i (+ s.len 1)])
      (s 0 i))))

(define inits string-prefixes)

(module+ test
  (check-equal? (string-prefixes "bar") (list "" "b" "ba" "bar")))

; matches ws xs = map length · filter (endswith ws) · inits xs
; inspired by R. Bird "Pearls of Functional Algorithm Design"
(define (string-matches1 pat text)
  ; the indices returned are ending positions !
  (with ([pat text :str])
    (map string-length
         (filter (λ* (pre :str) (pre.ends-with? pat))
                 (string-prefixes text)))))
(module+ test
  (check-equal? (string-matches1 "abcab" "ababcabcab") '(7 10)))

;;; Scan lemma
; (map (foldl op e (inits xs)) = (scanl op e xs)
(define (scanl op e xs)
  (let loop ([v e] [xs xs])
    (match xs
      ['()         (cons v '())]
      [(cons x xs) (cons v (loop (op v x) xs))])))

(module+ test
  (check-equal? (scanl / 64 '(4 2 4))  '(64 16 8 2)))

; (map f (filter p xs)) = (map first (filter second (map (fork f p) xs)))
; ((fork f p) x) = (cons (f x) (p x))
(define (fork f p) (λ (x) (list (f x) (p x))))

(define (string-matches2 pat text)
  ; the indices returned are ending positions !
  (with ([pat text :str])
    (map first
         (filter second
                 (map (fork string-length (λ* (pre :str) (pre.ends-with? pat)))
                      (string-prefixes text))))))

(module+ test
  (check-equal? (string-matches2 "abcab" "ababcabcab") '(7 10)))



