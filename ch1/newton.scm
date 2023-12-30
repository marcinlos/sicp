
(define tolerance 0.0001)

(define (fixed-point f guess)
  (define (close-enough a b)
    (< (abs (- a b)) tolerance))
  (define (try x)
    (let ((next (f x)))
      (if (close-enough x next)
          next
          (try next))))
  (try guess))

(define (fixed-point-of-transform f transform guess)
  (fixed-point (transform f) guess))

(define (newton f guess)
  (define dx 0.001)
  (define (deriv g)
    (lambda (x)
      (/ (- (g (+ x dx)) (g x)) dx)))
  (define (newton-transform g)
    (lambda (x)
      (- x (/ (g x) ((deriv g) x)))))
  (fixed-point-of-transform f newton-transform guess))


(define (compose f g)
  (lambda (x) (f (g x))))

(define (repeated f n)
  (if (= n 1)
      f
      (compose f (repeated f (- n 1)))))
