#lang sicp

(define (make-point x y)
  (cons x y))

(define x-point car)
(define y-point cdr)

(define (print-point p)
  (display "(")
  (display (x-point p))
  (display ", ")
  (display (y-point p))
  (display ")")
  (newline))

(define (make-segment p q)
  (cons p q))

(define start-segment car)
(define end-segment cdr)

(define (midpoint-segment s)
  (let ((p (start-segment s))
        (q (end-segment s)))
    (make-point (/ (+ (x-point p) (x-point q)) 2)
                (/ (+ (y-point p) (y-point q)) 2))))
