;; Exercise 2.48

(#%require sicp-pict)

(define outline-painter
  (let ((a (make-vect 0 0))
        (b (make-vect 1 0))
        (c (make-vect 1 1))
        (d (make-vect 0 1)))
    (segments->painter
     (list (make-segment a b)
           (make-segment b c)
           (make-segment c d)
           (make-segment d a)))))

(define x-painter
  (let ((a (make-vect 0 0))
        (b (make-vect 1 0))
        (c (make-vect 1 1))
        (d (make-vect 0 1)))
    (segments->painter
     (list (make-segment a c)
           (make-segment b d)))))

(define diamond-painter
  (let ((a (make-vect 0.0 0.5))
        (b (make-vect 0.5 0.0))
        (c (make-vect 1.0 0.5))
        (d (make-vect 0.5 1.0)))
    (segments->painter
     (list (make-segment a b)
           (make-segment b c)
           (make-segment c d)
           (make-segment d a)))))
