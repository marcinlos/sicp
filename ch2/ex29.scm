
(define (make-mobile left right)
  (list left right))

(define left-branch car)
(define right-branch cadr)

(define (make-branch length structure)
  (list length structure))

(define branch-length car)
(define branch-structure cadr)


(define (branch-weight branch)
  (let ((struct (structure-branch branch)))
    (if (number? struct)
        struct
        (total-weight struct))))

(define (total-weight mobile)
  (+ (branch-weight (left-branch mobile))
     (branch-weight (right-branch mobile))))

(define (mobile-balanced? mobile)
  (define (trivial-or-balanced? mobile)
    (if (number? mobile)
        #t
        (mobile-balanced? mobile)))
  (define (branch-torque branch)
    (* (branch-length branch)
       (branch-weight branch)))
  (let ((left (left-branch mobile))
        (right (right-branch mobile)))
    (if (= (branch-torque left)
           (branch-torque right))
        (and (trivial-or-balanced? left)
             (trivial-or-balanced? right))
        #f)))


(define (subsets set)
  (if (null? set)
      (list nil)
      (let ((rest (subsets (cdr set))))
        (append rest (map (lambda (x) (cons (car set) x)) rest)))))
