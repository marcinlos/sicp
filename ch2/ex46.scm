;; Exercise 2.46

(define (make-vect x y)
  (list x y))

(define (xcor-vect vect)
  (car vect))

(define (ycor-vect vect)
  (cadr vect))


(define (eq-vect? u v)
  (and (= (xcor-vect u) (xcor-vect v))
       (= (ycor-vect u) (ycor-vect v))))


(define (add-vect u v)
  (make-vect (+ (xcor-vect u) (xcor-vect v))
             (+ (ycor-vect u) (ycor-vect v))))


(define (sub-vect u v)
  (make-vect (- (xcor-vect u) (xcor-vect v))
             (- (ycor-vect u) (ycor-vect v))))


(define (scale-vect a v)
  (make-vect (* a (xcor-vect v))
             (* a (ycor-vect v))))


(define (test)
  (define (expect-eq a b)
    (if (not (eq-vect? a b))
        (begin
          (display "Not equal!")
          (newline)
          (display "  expected: ")
          (display b)
          (newline)
          (display "  got:      ")
          (display a)
          (newline)
          (error "Assertion failed"))))
  (expect-eq (add-vect (make-vect 1 3)
                       (make-vect 2 8))
             (make-vect 3 11))
  (expect-eq (sub-vect (make-vect 2 5)
                       (make-vect 4 3))
             (make-vect -2 2))
  (expect-eq (scale-vect 3 (make-vect 2 7))
             (make-vect 6 21)))
