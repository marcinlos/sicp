#lang planet neil/sicp


(define (cube-root x)

  (define (sqr x)
    (* x x))

  (define (cube x)
    (* x x x))

  (define (approx-error guess)
    (abs (- (cube guess) x)))

  (define (good-enough? guess)
    (< (approx-error guess) 0.001))

  (define (improve guess)
    (/ (+ (* 2 guess) (/ x (sqr guess))) 3))

  (define (cube-root-iter guess)
    (if (good-enough? guess)
        guess
        (cube-root-iter (improve guess))))

  (cube-root-iter 1.0))
