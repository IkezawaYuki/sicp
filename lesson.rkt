#lang racket

(define (cons x y)
  (lambda (m) (m x y)))

(define (car z)
  (z (lambda (p q) p)))

(define (cdr z)
  (z (lambda (p q) q)))

(define (fast-expt b n)
  (cond ((= n 0) 1)
        ((even? n) (square (fast-expt b (/ n 2))))
        (else (* b (fast-expt b (- n 1))))))

(define (even? n)
  (= (remainder n 2) 0))

(define (square x) (* x x))

(define (pcons x y)
  (* (fast-expt 2 x) (fast-expt 3 y)))

(define (pcar z)
  (if (= (remainder z 2) 0)
      (+ 1 (pcar (/ z 2)))
      0))

(define (pcdr z)
  (if (= (remainder z 3) 0)
      (+ 1 (pcdr (/ z 3)))
      0))











