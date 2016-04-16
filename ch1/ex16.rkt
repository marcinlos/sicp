#lang planet neil/sicp


(define (sqr x)
  (* x x))

(define (even? n)
  (= (remainder n 2) 0))

(define (power b n)
  (cond ((= n 0) 1)
        ((even? n) (sqr (power b (/ n 2))))
        (else (* b (power b (- n 1))))))

(define (power* b n)
  (define (power-iter n accum)
    (cond ((= n 0) accum)
          ((even? n) (power-iter (/ n 2) (sqr accum)))
          (else (power-iter (- n 1) (* b accum)))))
  (power-iter n 1))

