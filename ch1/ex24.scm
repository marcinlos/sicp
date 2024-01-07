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


(define (timed-prime-test test-method n)
  (define (start-prime-test start-time)
    (if (test-method n)
        (report-prime (- (runtime) start-time))))
  (define (report-prime elapsed-time)
    (display " *** ")
    (display (/ elapsed-time 1000.0)))
  (newline)
  (display n)
  (start-prime-test (runtime)))


(define (search-for-primes method from to)
  (define (search k)
    (if (< k to)
        (begin
          (timed-prime-test method k)
          (search (+ k 1)))))
  (search from))


(define (expmod b e m)
  (define (iter acc a k)
    (cond ((= 0 k) acc)
          ((even? k) (iter acc (remainder (square a) m) (/ k 2)))
          (else (iter (remainder (* a acc) m) a (- k 1)))))
  (iter 1 b e))


(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))


(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else false)))


(define (search-for-primes-slow from to)
  (search-for-primes prime? from to))

(define (search-for-primes-fast from to)
  (search-for-primes (lambda (n) (fast-prime? n 10)) from to))
