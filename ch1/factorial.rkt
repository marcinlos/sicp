#lang planet neil/sicp

(define (factorial n)
  (if (= n 1)
      1
      (* n (factorial (- n 1)))))

(define (factorial2 n)
  (define (factorial-iter n k)
    (if (= n 1)
        k
        (factorial-iter (- n 1) (* n k))))
  (factorial-iter n 1))

