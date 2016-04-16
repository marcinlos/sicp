#lang planet neil/sicp

(define (sqr x)
  (* x x))

;;; Test based on absolute value of error
(define (good-enough? guess x)
  (< (abs (- (sqr guess) x)) 0.001))

;;; Test based on error relative to argument
(define (rel-good-enough? guess x)
  (< (abs (/ (- (sqr guess) x) x)) 0.001))

(define (average x y)
  (/ (+ x y) 2))

(define (improve guess x)
  (average guess (/ x guess)))

(define (sqrt-iter guess x stop-condition)
  (if (stop-condition guess x)
      guess
      (sqrt-iter (improve guess x) x stop-condition)))

(define (sqrt x)
  (sqrt-iter 1.0 x rel-good-enough?))
