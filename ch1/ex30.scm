
; Iterative summation
(define (sum-iter term a next b)
  (define (go acc a)
    (if (> a b)
        acc
        (go (+ (term a) acc) (next a))))
  (go 0 a))

