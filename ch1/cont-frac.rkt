#lang sicp

(define (cont-frac N D k)
  (define (cont-frac-iter i)
    (if (> i k)
        0
        (/ (N i)
           (+ (D i)
              (cont-frac-iter (+ i 1))))))
  (cont-frac-iter 1))

(define (cont-frac* N D k)
  (define (cont-frac-iter i acc)
    (if (= i 0)
        acc
        (cont-frac-iter (- i 1)
                        (/ (N i)
                           (+ (D i) acc)))))
  (cont-frac-iter k 0))

(define e-minus-2-approx
  (cont-frac (lambda (i) 1.0)
             (lambda (i)
               (cond ((> (remainder (+ i 1) 3) 0) 1.0)
                     (else (* 2 (/ (+ i 1) 3)))))
             100))
