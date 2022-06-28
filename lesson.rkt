#lang racket
(define (inc a) (+ a 1))
(define (dec a) (- a 1))

(define (+ a b)
  (if (= a 0)
      b
      (inc (+ (dec a) b))))






