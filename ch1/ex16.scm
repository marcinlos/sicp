
(define (square x)
  (* x x))

(define (fast-expt-iter b n)
  (define (iter k acc a)
    (cond ((= k 0) acc)
          ((even? k) (iter (/ k 2) acc (square a)))
          (else (iter (- k 1) (* a acc) a))))
  (iter n 1 b))
