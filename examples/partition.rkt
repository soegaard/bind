#lang racket
(require math/base)

;;;
;;; Hoare's Selection Algorithm
;;; 

; http://en.wikipedia.org/wiki/Selection_algorithm
; #Partition-based_general_selection_algorithm

;function partition(list, left, right, pivotIndex)
;     pivotValue := list[pivotIndex]
;     swap list[pivotIndex] and list[right]  // Move pivot to end
;     storeIndex := left
;     for i from left to right-1
;         if list[i] < pivotValue
;             swap list[storeIndex] and list[i]
;             increment storeIndex
;     swap list[right] and list[storeIndex]  // Move pivot to its final place
;     return storeIndex

(require bind)

(define (partition! ys pi [l 0] [r (vector-length ys)])
  (def xs :vector ys)
  (def r-1 (- r 1))
  (define-syntax-rule (swap i j)
    (let ([t (xs i)]) (xs! i (xs j)) (xs! j t)))
  (def p (xs pi))
  (swap pi r-1)
  (def j l)
  (for ([i (in-range l r)])
    (when (< (xs i) p)
      (swap j i)
      (set! j (+ j 1))))
  (swap r-1 j)
  j)

(partition! (vector 0 1 2 9 3 4 6 7 8 5) 5)

;function select(list, left, right, k)
;     if left = right // If the list contains only one element
;         return list[left]  // Return that element
;     // select pivotIndex between left and right
;     pivotNewIndex := partition(list, left, right, pivotIndex)
;     pivotDist := pivotNewIndex - left + 1
;     // The pivot is in its final sorted position,
;     // so pivotDist reflects its 1-based position if list were sorted
;     if pivotDist = k
;         return list[pivotNewIndex]
;     else if k < pivotDist
;         return select(list, left, pivotNewIndex - 1, k)
;     else
;         return select(list, pivotNewIndex + 1, right, k - pivotDist)

(define (select ys k [l 0] [r (vector-length ys)])
  (def xs :vector ys)
  (def r-1 (- r 1))
  (cond [(= l r-1) (xs l)]
        [else
         (displayln (list l r))
         (def pi (random-integer l r))
         (def new-pi (partition! xs pi l r))
         (def dist (+ new-pi (- l) 1))
         (cond [(= dist k) (xs new-pi)]
               [(< k dist) (select xs k l pi)]
               [else       (select xs (- k dist) pi (add1 r))])]))

(for/list ([k 9])
  (select (vector 0 1 2 9 3 4 6 7 8 5) k))

