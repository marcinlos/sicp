#lang sicp

(define (even? n)
  (= (remainder n 2) 0))

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(define (sum-of-squares a b)
  (sum (lambda (n) (* n n)) a (lambda (n) (+ n 1)) b))

(define (simpson f a b n)
  (let ((h (/ (- b a) n)))
    (define (coeff i)
      (cond ((or (= i 0) (= i n)) 1)
            ((even? i) 2)
            (else 4)))
    (define (term i)
      (let ((x (+ a (* i h (- b a)))))
        (* (/ h 3) (f x) (coeff i))))
    (sum term 0 inc n)))
