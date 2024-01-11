;; Exercise 2.3

(load "ch2/points.scm")

; Representation

(define (make-rect p1 p2)
  (define (in-order x y)
    (if (> x y)
        (cons y x)
        (cons x y)))
  (let ((ord-x (in-order (x-point p1)
                         (x-point p2)))
        (ord-y (in-order (y-point p1)
                         (y-point p2))))
    (let ((left-bottom (make-point (car ord-x)
                                   (car ord-y)))
          (right-top (make-point (cdr ord-x)
                                 (cdr ord-y))))
      (cons left-bottom right-top))))


(define (left-rect r)
  (x-point (car r)))


(define (right-rect r)
  (x-point (cdr r)))


(define (bottom-rect r)
  (y-point (car r)))


(define (top-rect r)
  (y-point (cdr r)))


; Code using the representation


(define (width-rect r)
  (- (right-rect r)
     (left-rect r)))


(define (height-rect r)
  (- (top-rect r)
     (bottom-rect r)))


(define (area-rect r)
  (* (width-rect r)
     (height-rect r)))


(define (perimeter-rect r)
  (* 2 (+ (width-rect r)
          (height-rect r))))


(define (print-rect r)
  (display "[")
  (display (x-point (car r)))
  (display ", ")
  (display (x-point (cdr r)))
  (display "]")
  (display " x ")
  (display "[")
  (display (y-point (car r)))
  (display ", ")
  (display (y-point (cdr r)))
  (display "]"))


; Test

(define (test-rectangles)
  (let ((r (make-rect (make-point 1 5) (make-point 3 4))))
    (print-rect r)
    (newline)
    (display "Height: ")
    (display (height-rect r))
    (newline)
    (display "Width: ")
    (display (width-rect r))
    (newline)
    (display "Area: ")
    (display (area-rect r))
    (newline)
    (display "Perimiter: ")
    (display (perimeter-rect r))))
