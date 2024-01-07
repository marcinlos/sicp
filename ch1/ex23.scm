(define (square n) (* n n))

(define (divides? d n) (= 0 (remainder n d)))

(define (smallest-divisor n)
  (define (next k)
    (cond ((= k 2) 3)
          (else (+ 2 k))))
  (define (find-divisor test-divisor)
    (cond ((> (square test-divisor) n) n)
          ((divides? test-divisor n) test-divisor)
          (else (find-divisor (next test-divisor)))))
  (find-divisor 2))

(define (prime? n)
  (= (smallest-divisor n) n))


(define (timed-prime-test n)
  (define (start-prime-test start-time)
    (if (prime? n)
        (report-prime (- (runtime) start-time))))
  (define (report-prime elapsed-time)
    (display " *** ")
    (display (/ elapsed-time 1000.0)))
  (newline)
  (display n)
  (start-prime-test (runtime)))


(define (search-for-primes from to)
  (define (search k)
    (if (< k to)
        (begin
          (timed-prime-test k)
          (search (+ k 1)))))
  (search from))
