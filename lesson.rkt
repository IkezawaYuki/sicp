#lang racket

(define (square x) (* x x))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m))
                    m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))

(define (carmichael-test n) ;;returns n-1 for carmichael number n
  (define (ctest a i)
    (cond ((= a n) i)
          ((= (expmod a n n) a) (ctest (+ a 1) (+ i 1)))
          (else (ctest (+ a 1) i))))
  (ctest 1 0))

(define (cube x) (* x x x))

(define (sum-integers a b)
  (if (> a b)
      0
      (+ a (sum-integers (+ a 1) b))))

(define (sum-cubes a b)
  (if (> a b)
      0
      (+ (cube a) (sum-cubes (+ a 1) b)