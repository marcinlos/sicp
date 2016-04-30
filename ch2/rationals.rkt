#lang sicp

(define (gcd a b)
  (if (zero? b)
      a
      (gcd (remainder a b) b)))

(define (make-rat n d)
  (let ((sign (if (negative? (* n d)) -1 1))
        (k (gcd n d)))
    (cons (/ (* sign n) k)
          (/ (abs d) k))))

(define (numer r)
  (car r))

(define (denom r)
  (cdr r))


(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (sub-rat x y)
  (make-rat (- (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))

(define (div-rat x y)
  (make-rat (* (numer x) (denom y))
            (* (denom x) (numer y))))

(define (equal-rat? x y)
  (= (* (numer x) (denom y))
     (* (denom x) (numer y))))

(define (print-rat r)
  (display (numer r))
  (display "/")
  (display (denom r))
  (newline))
