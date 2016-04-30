#lang sicp

(define tolerance 0.0001)

(define (find-fixed f guess)
  (define (close-enough a b)
    (< (abs (- a b)) tolerance))
  (define (try x)
    (let ((next (f x)))
      (display x)
      (newline)
      (if (close-enough x next)
          next
          (try next))))
  (try guess))
