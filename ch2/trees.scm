
(define (fold-tree f init t)
  (define (step t acc)
    (cond ((null? t) acc)
          ((not (pair? t)) (f acc t))
          (else (let ((new-acc (step (car t) acc)))
                  (step (cdr t) new-acc)))))
  (step t init))

(define (map-tree f t)
  (cond ((null? t) nil)
        ((not (pair? t)) (f t))
        (else (cons (map-tree f (car t))
                    (map-tree f (cdr t))))))

(define (count-leaves t)
  (cond ((null? t) 0)
        ((not (pair? t)) 1)
        (else (+ (count-leaves (car t))
                 (count-leaves (cdr t))))))

(define (fringe t)
  (cond ((null? t) nil)
        ((not (pair? t)) (list t))
        (else (append (fringe (car t))
                      (fringe (cdr t))))))

(define (accumulate op init seq)
  (if (null? seq)
      init
      (op (car seq) (accumulate op init (cdr seq)))))

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      nil
      (cons (accumulate op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))

(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(define (matrix-*-vector m v)
  (map (lambda (u) (dot-product u v)) m))

(define (transpose-matrix m)
  (accumulate-n cons nil m))

(define (matrix-*-matrix m n)
  (map (lambda (u) (matrix-*-vector m u))
       (transpose-matrix n)))

(define (flatmap f m)
  (accumulate append nil (map f m)))

(define (filter p m)
  (if (null? m)
      nil
      (let ((rest (filter p (cdr m))))
        (if (p (car m))
            (cons (car m) rest)
            rest))))

(define (remove x items)
  (filter (lambda (a) (not (= a x))) items))

(define (permutations items)
  (if (null? items)
      (list nil)
      (flatmap (lambda (x) (map (lambda (xs) (cons x xs))
                                (permutations (remove x items)))) items)))
