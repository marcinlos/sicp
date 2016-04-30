#lang sicp

;; List utilities

(define (accumulate op init seq)
  (if (null? seq)
      init
      (op (car seq) (accumulate op init (cdr seq)))))

(define (flatmap f m)
  (accumulate append nil (map f m)))

(define (filter p m)
  (if (null? m)
      nil
      (let ((rest (filter p (cdr m))))
        (if (p (car m))
            (cons (car m) rest)
            rest))))

(define (contains x seq)
  (cond ((null? seq) #f)
        ((= x (car seq)) #t)
        (else (contains x (cdr seq)))))

(define (remove x items)
  (filter (lambda (a) (not (= a x))) items))


(define empty-board nil)

(define (adjoin-position row col board)
  (cons (cons row col) board))

(define pos-row car)
(define pos-col cdr)

(define (positive-diag p)
  (- (pos-row p) (pos-col p)))

(define (negative-diag p)
  (+ (pos-row p) (pos-col p)))

(define (safe? pos board)
  (define (occupied what)
    (contains (what pos)
              (map what board)))
  (not (or (occupied pos-row)
           (occupied pos-col)
           (occupied positive-diag)
           (occupied negative-diag))))

(define (most-recent-queen-safe? board)
    (safe? (car board) (cdr board)))

(define (enumerate-interval a b)
  (if (> a b)
      nil
      (cons a (enumerate-interval (inc a) b))))

(define (queens board-size)
  (define (queen-cols k)
    (if (= 0 k)
        (list empty-board)
        (filter most-recent-queen-safe?
                (flatmap
                 (lambda (board)
                   (map (lambda (n) (adjoin-position n k board))
                        (enumerate-interval 1 board-size)))
                 (queen-cols (- k 1))))))
  (queen-cols board-size))
