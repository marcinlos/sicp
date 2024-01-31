(define (is-op? op exp)
  (and (pair? exp) (equal? (car exp) op)))

(define (sum? exp)
  (is-op? '+ exp))

(define (is-num-eq? exp n)
  (and (number? exp) (= exp n)))

(define (make-sum a b)
  (cond ((is-num-eq? a 0) b)
        ((is-num-eq? b 0) a)
        (else (list '+ a b))))

(define (addend exp) (cadr exp))
(define (augend exp) (let ((rest (cddr exp)))
                       (if (null? (cdr rest))
                           (car rest)
                           (cons '+ (cddr exp)))))

(define (product? exp)
  (is-op? '* exp))


(define (make-product a b)
  (cond ((is-num-eq? a 1) b)
        ((is-num-eq? b 1) a)
        ((or (is-num-eq? a 0) (is-num-eq? b 0)) 0)
        ((and (number? a) (number? b)) (* a b))
        (else (list '* a b))))

(define (multiplier exp) (cadr exp))
(define (multiplicand exp) (let ((rest (cddr exp)))
                             (if (null? (cdr rest))
                                 (car rest)
                                 (cons '* rest))))


(define (exponentiation? exp)
  (is-op? '** exp))

(define (make-exponentiation a b)
  (cond ((is-num-eq? a 1) 1)
        ((is-num-eq? b 0) 1)
        ((is-num-eq? b 1) a)
        (else (list '** a b))))

(define (base exp) (cadr exp))
(define (exponent exp) (caddr exp))

(define variable? symbol?)

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (equal? v1 v2)))

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        ((sum? exp) (make-sum (deriv (addend exp) var)
                              (deriv (augend exp) var)))
        ((product? exp)
         (make-sum (make-product (multiplier exp)
                                 (deriv (multiplicand exp) var))
                   (make-product (deriv (multiplier exp) var)
                                 (multiplicand exp))))
        ((exponentiation? exp)
         (let ((a (base exp))
               (b (exponent exp)))
           (make-product exp
                         (make-sum (make-product (deriv b var) (list 'log base))
                                   (make-product (deriv a var) (list '/ b a))))))
        (else (error "Unsupported expression: " exp))))
