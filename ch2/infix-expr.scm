;; Exercise 2.58b

;;; Recursive descent parser for a simple infix expression grammar
;;;
;;; expr = term + term
;;; term = factor * factor
;;;
(define (parse-to-prefix symbols)
  (car (parse-expr symbols)))


(define (parse-expr symbols)
  (let* ((result (parse-term symbols))
         (term (car result))
         (rest (cdr result)))
    (cond ((null? rest) result)
          ((equal? (car rest) '+)
           (let* ((result (parse-expr (cdr rest)))
                  (expr (car result))
                  (rest (cdr result)))
             (cons (list '+ term expr) rest)))
          (else (cons term rest)))))


(define (parse-term symbols)
  (let* ((result (parse-factor symbols))
         (factor (car result))
         (rest (cdr result)))
    (cond ((null? rest) result)
          ((equal? (car rest) '*)
           (let* ((result (parse-term (cdr rest)))
                  (term (car result))
                  (rest (cdr result)))
             (cons (list '* factor term) rest)))
          (else (cons factor rest)))))


(define (parse-factor symbols)
  (define (try options)
    (if (null? options)
        (error "syntax error")
        (let ((pred (car options)))
          (if (pred (car symbols))
              symbols
              (try (cdr options))))))
  (if (pair? (car symbols))
      (cons (parse-expr (car symbols))
            (cdr symbols))
      (try (list symbol? number?))))
