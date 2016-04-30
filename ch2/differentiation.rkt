#lang sicp

(define (sum? exp)
  (and (pair? exp) (equal? (car exp) '+)))

(define (is-num-eq? exp n)
  (and (number? exp) (= exp n)))

(define (make-sum a b)
  (cond ((is-num-eq? a 0) b)
        ((is-num-eq? b 0) a)
        (else (list '+ a b))))

(define (addend exp) (cadr exp))
(define (augend exp) (caddr exp))

(define (product? exp)
  (and (pair? exp) (equal? (car exp) '*)))

(define (make-product a b)
  (cond ((is-num-eq? a 1) b)
        ((is-num-eq? b 1) a)
        ((or (is-num-eq? a 0) (is-num-eq? b 0)) 0)
        ((and (number? a) (number? b)) (* a b))
        (else (list '* a b))))

(define (multiplier exp) (cadr exp))
(define (multiplicand exp) (caddr exp))

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
                                 (multiplicand exp))))))
