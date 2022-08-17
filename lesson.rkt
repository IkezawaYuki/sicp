#lang racket

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (map p sequence)
  (accumulate (lambda (x y)
                (cons (p x) y)) '() sequence))

(map (lambda (x) (* x x)) '(0 1 2 3))

(define (append seq1 seq2)
  (accumulate cons seq2 seq1))

