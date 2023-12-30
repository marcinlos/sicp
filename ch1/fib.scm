
(define (fib n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (fib (- n 1))
                 (fib (- n 2))))))

(define (fib2 n)
  (define (fib-iter k a b)
    (if (< k n)
        (fib-iter (+ k 1) (+ a b) a)
        a))
  (fib-iter 0 0 1))


