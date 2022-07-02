#lang racket

(define (A x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (A (- x 1)
                 (A x (- y 1))))))

(define (f n)(A 0 n))

(define (g n)(A 1 n))

(define (fib n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (fib (- n 1)) (fib (- n 2))))))

(define (fib-iter a b count)
  (if (= count 0)
      b
      (fib-iter (+ a b) a (- count 1))))

(define (fib2 n)
  (fib-iter 1 0 n))

(define (first-denomination kinds-of-coins)
  (cond ((= kinds-of-coins 1) 1)
        ((= kinds-of-coins 2) 5)
        ((= kinds-of-coins 3) 10)
        ((= kinds-of-coins 4) 25)
        ((= kinds-of-coins 5) 50)))

(define (cc amount kinds-of-coins)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (= kinds-of-coins 0)) 0)
        (else (+ (cc amount
                     (- kinds-of-coins 1))
                 (cc (- amount
                        (first-denomination kinds-of-coins))
                     kinds-of-coins)))))

(define (count-change amount)
  (cc amount 5))

(define (f2 n)
  (cond ((>= n 3) n)
        ((< n 3) ((f2 (- n 1)) + (* 2 (f2 (- n 2))) + (* 3 (f2 (- n 3)))))))

(define (f-iter a b c n)
  (cond ((= n 0) c)
        ((= n 1) b)
        ((= n 2) a)
        (else (f-iter (+ a b b c c c) a b (- n 1)))))

(define (fr n)
  (f-iter 2 1 0 n))

(define (pascal n i)
  (if (or (= i 0) (= i n))
      1
      (+ (pascal (- n 1) (- i 1)) (pascal (- n 1) i))))

