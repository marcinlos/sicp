#lang planet neil/sicp

(define (sqr x)
  (* x x))


(define (expmod base exp n)
  (cond ((= exp 0) 1)
        ((even? exp)
         (let* ((r (expmod base (/ exp 2) n))
                (sqr (remainder (sqr r) n)))
           (if (and (= sqr 1)
                    (not (or (= r 1) (= r (- n 1)))))
               0
               sqr)))
        (else
         (remainder (* base (expmod base (- exp 1) n))
                    n))))

(define (miller-rabin-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (random (- n 1))))

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((miller-rabin-test n) (fast-prime? n (- times 1)))
        (else false)))
