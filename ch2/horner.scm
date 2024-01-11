;; Exercise 2.34

(define (accumulate op start items)
  (if (null? items)
      start
      (op (car items)
          (accumulate op start (cdr items)))))


(define (horner-eval x coeffs)
  (accumulate (lambda (a r) (+ a (* r x)))
              0
              coeffs))


(define (test)
  (define (assert-eq a b)
    (if (not (= a b))
        (begin
          (display "Got: ")
          (display a)
          (newline)
          (display "Expected:")
          (display b)
          (newline)
          (error "Not equal!"))))
  (assert-eq (horner-eval 2 '(3 5 -1))
             9))
