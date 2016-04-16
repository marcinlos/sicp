#lang planet neil/sicp

(define (even? n)
  (= (remainder n 2) 0))


(define (fib n)
  (define (fib-iter n a b p q)
    (cond ((= n 0) b)
          ((even? n) (let ((pp (+ (* p p) (* q q)))
                           (qq (+ (* q q) (* 2 p q))))
                       (fib-iter (/ n 2) a b pp qq)))
          (else (let ((aa (+ (* b q) (* a q) (* a p)))
                      (bb (+ (* b p) (* a q))))
                  (fib-iter (- n 1) aa bb p q)))))
  (fib-iter n 1 0 0 1))

