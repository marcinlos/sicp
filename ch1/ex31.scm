
; iterative process
(define (product term a next b)
  (define (go acc a)
    (if (> a b)
        acc
        (go (* (term a) acc)
            (next a))))
  (go 1 a))

; recursive process
(define (product-rec term a next b)
  (if (> a b)
      1
      (* (term a)
         (product-rec term (next a) next b))))


(define (prod-pi n)
  (define (pi-term a)
    (/ (* (- a 1) (+ a 1))
       (* a a)))
  (define (pi-next a)
    (+ a 2))
  (product pi-term 3.0 pi-next n))
