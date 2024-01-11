;; Exercise 2.39

(define (fold-right op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (fold-right op initial (cdr sequence)))))


(define (fold-left op initial sequence)
  (define (loop acc items)
    (if (null? items)
        acc
        (loop (op acc (car items))
              (cdr items))))
  (loop initial sequence))


(define (reverse-right sequence)
  (fold-right (lambda (x y) (append y (list x)))
              nil
              sequence))


(define (reverse-left sequence)
  (fold-left (lambda (x y) (cons y x))
             nil
             sequence))
