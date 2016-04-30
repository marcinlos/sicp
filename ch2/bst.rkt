#lang sicp

(define empty-tree nil)

(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))

(define (make-tree entry left right)
  (list entry left right))

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((= x (entry set)) true)
        ((< x (entry set)) (element-of-set? x (left-branch set)))
        ((> x (entry set)) (element-of-set? x (right-branch set)))))

(define (adjoin-set x set)
  (cond ((null? set) (make-tree x empty-tree empty-tree))
        ((= x (entry set)) set)
        ((< x (entry set)) (make-tree (entry set)
                                      (adjoin-set x (left-branch set))
                                      (right-branch set)))
        ((> x (entry set)) (make-tree (entry set)
                                      (left-branch set)
                                      (adjoin-set x (right-branch set))))))


(define (foldr op init seq)
  (if (null? seq)
      init
      (op (car seq) (foldr op init (cdr seq)))))

(define (tree->list tree)
  (let step ((t tree) (acc '()))
    (if (null? t)
        acc
        (step (left-branch t)
              (cons (entry t)
                    (step (right-branch t) acc))))))

