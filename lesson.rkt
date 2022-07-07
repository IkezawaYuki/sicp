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
      (+ (cube a) (sum-cubes (+ a 1) b))))

(define (pi-sum a b)
  (if (> a b)
      0
      (+ (/ 1.0 (* a (+ a 2))) (pi-sum (+ a 4) b))))

(define (inc c)(+ c 1))

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(define (sum-cubes2 a b)
  (sum cube a inc b))

(define (identity x) x)

(define (sum-integers2 a b)
  (sum identity a inc b))

(define (pi-sum2 a b)
  (define (pi-term x)
    (/ 1.0 (* x (+ x 2))))
  (define (pi-next x)
    (+ x 4))
  (sum pi-term a pi-next b))


(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
    (* (sum f (+ a (/ dx 2.0)) add-dx b)
         dx))

(define (simpson f a b n)
  (define h (/ (- b a) n))
  (define (simpson-term x) (+ (f x)
                              (* 4 (f (+ x h)))
                              (f (+ x h h))))
  (define (simpson-next x) (+ x h h))
  (/ (* h (sum simpson-term a simpson-next (- b h)))
     3.0))

(define (sum2 term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ (term a) result))))
  (iter a 0))


(define (factorial n)
  (product identity 1 inc n))

(display (factorial 10)) (newline)

