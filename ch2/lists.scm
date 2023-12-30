
(define (length* list)
  (if (null? list)
      0
      (+ 1 (length* (cdr list)))))

(define (sum* list)
  (if (null? list)
      0
      (+ (car list) (sum* (cdr list)))))

(define (sum-it* list)
  (define (sum-iter list sum)
    (if (null? list)
        sum
        (sum-iter (cdr list) (+ sum (car list)))))
  (sum-iter list 0))

(define (fold* init f list)
  (define (fold-iter list acc)
    (if (null? list)
        acc
        (fold-iter (cdr list) (f acc (car list)))))
  (fold-iter list init))

(define (append* list1 list2)
  (if (null? list1)
      list2
      (cons (car list1) (append* (cdr list1) list2))))

(define (last-pair list)
  (if (null? (cdr list))
      list
      (last-pair (cdr list))))

(define (all-match p list)
  (cond ((null? list) #t)
        ((p (car list)) (all-match p (cdr list)))
        (else #f)))

(define (same-parity x . rest)
  (all-match (if (odd? x)
                 odd?
                 even?)
             rest))

(define (foldr* init f list)
  (if (null? list)
      init
      (f (car list)
         (foldr* init f (cdr list)))))

(define (map* f list)
  (foldr* nil (lambda (x tail) (cons (f x) tail)) list))

(define (reverse* list)
  (define (reverse-iter list new-list)
    (if (null? list)
        new-list
        (reverse-iter (cdr list) (cons (car list) new-list))))
  (reverse-iter list nil))

(define (map-reverse* f list)
  (define (map-reverse-iter f list new-list)
    (if (null? list)
        new-list
        (map-reverse-iter f (cdr list) (cons (f (car list))
                                             new-list))))
  (map-reverse-iter f list nil))

(define (deep-reverse* list)
  (if (pair? list)
      (map-reverse* deep-reverse* list)
      list))
