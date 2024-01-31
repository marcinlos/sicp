;; Exercise 2.54

(define (equal? a b)
  (cond ((and (pair? a) (pair? b)) (and (equal? (car a) (car b))
                                        (equal? (cdr a) (cdr b))))
        ((and (symbol? a) (symbol? b)) (eq? a b))
        ((and (eq? a '()) (eq? b '())) true)
        (else false)))


(define (test)
  (define (print-example a b expected)
    (let ((result (equal? a b)))
      (display a)
      (display
        (if result
            " == "
            " != "))
      (display b)
      (newline)
      (display
        (if (eq? result expected)
            "OK"
            "ERROR"))
      (newline)))
  (print-example 'a 'a true)
  (print-example 'a 'b false)
  (print-example '(a b) '(a b) true)
  (print-example '(a b) '(a b c) false)
  (print-example '(a (b c) d) '(a (b c) d) true))
