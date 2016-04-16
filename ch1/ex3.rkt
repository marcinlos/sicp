#lang planet neil/sicp

(define (sqr x)
  (* x x))

(define (sum-of-squares x y)
  (+ (sqr x) (sqr y)))

(define (two-largest x y z)
  (if (> x y)
      (if (> z y)
          (list x z)
          (list x y))
      (if (> z x)
          (list y z)
          (list y x))))

(define (sum-of-two-largest-squares x y z)
  (apply sum-of-squares (two-largest x y z)))

