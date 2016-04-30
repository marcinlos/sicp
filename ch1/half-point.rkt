#lang sicp


(define (bisect f pos-point neg-point)
  (define (average a b)
    (/ (+ a b) 2))
  (define (close-enough x y)
    (< (abs (- x y)) 0.001))
  (let ((m (average pos-point neg-point)))
    (if (close-enough pos-point neg-point)
        m
        (let ((m-val (f m)))
          (cond ((negative? m-val) (bisect f pos-point m))
                ((positive? m-val) (bisect f m neg-point))
                (else m))))))

(define (find-root f a b)
  (let ((a-val (f a))
        (b-val (f b)))
    (cond ((and (positive? a-val) (negative? b-val))
           (bisect f a b))
          ((and (negative? a-val) (positive? b-val))
           (bisect f b a))
          (else
           (display "Error!!!!\n")))))
