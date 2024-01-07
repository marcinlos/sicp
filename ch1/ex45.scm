
(define tolerance 0.0001)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2))
       tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))


(define (fixed-point-of-transform g transform guess)
  (fixed-point (transform g) guess))


(define (average a b)
  (/ (+ a b) 2))

(define (average-damp f)
  (lambda (x) (average x (f x))))


(define (sqrt x)
  (fixed-point-of-transform (lambda (y) (/ x y)) average-damp 1.0))


(define (power x n)
  (define (go acc k)
    (if (> k n)
        acc
        (go (* x acc) (+ k 1))))
  (go 1.0 1))


(define (cube-root x)
  (fixed-point-of-transform (lambda (y) (/ x (power y 2)))
                            average-damp
                            1.0))

(define (compose f g)
  (lambda (x) (f (g x))))


(define (iterated f n)
  (lambda (x) (if (= n 0)
                  x
                  (f ((iterated f (- n 1)) x)))))

(define (floor x)
  (inexact->exact (truncate x)))

(define (log2 x)
  (floor (/ (log x) (log 2))))


; Number of required average damp iterations grows logarithmically
; with the root degree.
;
; Newton method computes the n-th root of a as a fixed point of
;
; f(x) = x - (x^n - a) / (n x^(n -1))
;      = (1 - 1/n) x + 1/n a/x^(n-1)
;
; while k average dampings result in
;
; g(x) = (1 - 2^(-k)) x + 2^(-k) a/x^(n-1))
;
; so we need 2^k ~ n to match that.
(define (root x n)
  (fixed-point-of-transform (lambda (y) (/ x (power y (- n 1))))
                            (iterated average-damp (log2 n))
                            1.0))
