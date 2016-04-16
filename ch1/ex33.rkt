#lang sicp

(define (accumulate-filter* combiner pred null-value term a next b)
  (define (accumulate-iter a result)
    (if (> a b)
        result
        (let ((new-result (if (pred a)
                              (combiner (term a) result)
                              result)))
          (accumulate-iter (next a) new-result))))
  (accumulate-iter a null-value))

