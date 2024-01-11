

(define (make-interval x y) (cons (min x y)
                                  (max x y)))
(define lower-bound car)
(define upper-bound cdr)

(define (print-interval interval)
  (display "[")
  (display (lower-bound interval))
  (display ", ")
  (display (upper-bound interval))
  (display "]")
  (newline))

(define (make-center-width center width)
  (make-interval (- center width)
                 (+ center width)))

(define (center i)
  (/ (+ (upper-bound i) (lower-bound i)) 2))

(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))

(define (make-center-percent center percent)
  (make-center-width center (* center percent)))

(define (tolerance-percent i)
  (/ (width i) (center i)))


(define (add-interval a b)
  (make-interval (+ (lower-bound a) (lower-bound b))
                 (+ (upper-bound a) (upper-bound b))))

(define (sub-interval a b)
  (make-interval (- (lower-bound a) (upper-bound b))
                 (- (upper-bound a) (lower-bound b))))

(define (mul-interval a b)
  (let ((ends (list (* (lower-bound a) (lower-bound b))
                    (* (upper-bound a) (lower-bound b))
                    (* (upper-bound a) (upper-bound b))
                    (* (lower-bound a) (upper-bound b)))))
    (make-interval (apply min ends) (apply max ends))))

(define (inv-interval i)
  (if (> 0 (* (lower-bound i) (upper-bound i)))
      (error "Interval spans 0"))
  (make-interval (/ 1.0 (upper-bound i))
                 (/ 1.0 (lower-bound i))))
