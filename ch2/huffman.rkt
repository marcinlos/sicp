#lang sicp

(define (make-leaf symbol weight)
  (list 'leaf symbol weight))

(define (leaf? node)
  (eq? (car node) 'leaf))

(define symbol-leaf cadr)
(define weight-leaf caddr)

(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))

(define (symbols tree)
  (if (leaf?)
      (list (symbol-leaf tree))
      (caddr tree)))

(define (weight tree)
  (if (leaf?)
      (weight-leaf tree)
      (cadddr tree)))

(define (frequency-list string)
  (define char car)
  (define count cdr)
  (define (inc-count entry)
    (cons (char entry) (+ 1 (count entry))))
  (define (increment c freq-list)
    (cond ((null? freq-list)
           (list (cons c 1)))
          ((equal? (char (car freq-list)) c)
           (cons (inc-count (car freq-list)) (cdr freq-list)))
          (else (cons (car freq-list) (increment c (cdr freq-list))))))
  (let build ((s (string->list string)) (freq-list nil))
    (if (null? s)
        freq-list
        (build (cdr s) (increment (car s) freq-list)))))

(define (make-huffman-tree string)
  (define (remove item list)
    (if (equal? item (car list))
        (cdr list)
        (cons (car list) (remove item (cdr list)))))
  (define (find-minimal nodes)
    )
  (define (remove-minimal-node nodes)
    ))
