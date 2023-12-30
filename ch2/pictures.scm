
(#%require sicp-pict)

(define (split first second)
  (define (splitter painter n)
    (if (= n 0)
        painter
        (let ((smaller (splitter painter (- n 1))))
          (first painter (second smaller smaller)))))
  splitter)

(define right-split (split beside below))

(define up-split (split below beside))

(define (corner-split painter n)
  (if (= n 0)
      painter
      (let ((smaller-up (up-split painter (- n 1)))
            (smaller-right (right-split painter (- n 1)))
            (smaller-corner (corner-split painter (- n 1))))
        (beside
          (below painter (beside smaller-up smaller-up))
          (below (below smaller-right smaller-right) smaller-corner)))))

(define (square-limit painter n)
  (let* ((quarter (corner-split painter n))
         (half (beside (flip-horiz quarter) quarter)))
    (below (flip-vert half) half)))

(define (square-of-four tl tr bl br)
  (lambda (painter)
    (below (beside (bl painter) (br painter))
           (beside (tl painter) (tr painter)))))

(define (square-limit2 painter n)
  (let ((combiner (square-of-four flip-horiz identity
                                  rotate180 flip-vert)))
    (combiner (corner-split painter n))))
