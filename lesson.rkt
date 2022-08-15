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