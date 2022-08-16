#lang racket

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define sequence (list 1 2 3 4 5))

sequence

(car sequence)

(cdr sequence)

(define (horner-eval x coefficient-sequence)
  (accumulate
   (lambda (this-coeff higer-terms)
     (+ (* higer-terms x) this-coeff))
   0
   coefficient-sequence))
