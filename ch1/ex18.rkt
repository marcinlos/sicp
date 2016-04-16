#lang planet neil/sicp


(define (double n)
  (+ n n))

(define (halve n)
  (/ n 2))

(define (russian-multiply n m)
  (cond ((= n 1) m)
        ((even? n) (double (russian-multiply (halve n) m)))
        (else (+ m (russian-multiply (- n 1) m)))))

(define (russian-multiply* n m)
  (define (russian-iter n m accum)
    (cond ((= n 1) (+ m accum))
          ((even? n) (russian-iter (halve n) (double m) accum))
          (else (russian-iter (- n 1) m (+ m accum)))))
  (russian-iter n m 0))
