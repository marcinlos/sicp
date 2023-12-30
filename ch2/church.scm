
(define (church-eval n)
  ((n inc) 0))

(define zero* (lambda (f) (lambda (x) x)))

(define (inc* n)
  (lambda (f)
    (lambda (x) (f ((n f) x)))))

(define (add* n m)
  (lambda (f)
    (lambda (x) ((n f) ((m f) x)))))

(define (mul* n m)
  (lambda (f)
    (lambda (x) ((n (m f)) x))))
