#lang racket
(/ 6 2)
(+ (* 2 4) (- 4 8))
(define a 3)
(define b (+ a 1))
(= a b)
(if (and (> b a)(< b (* a b)))
         b
         a)

(cond ((= a 4) 6)
      ((= b 4) (+ 6 7 a))
      (else 25))
(+ 2(if (> b a) b a))

(* (cond ((> a b) a)
         ((< a b) b)
         (else -1))
   (+ a 1))

(/ (* 3 (- 6 2)(- 2 7))(+ (+ 5 4)(- 2(- 3 (+ 6 (/ 4 5))))))



(define (func a b c)
  (cond ((and (>= b a) (>= c a)) (+ (* b b) (* c c)))
        ((and (>= a b) (>= c b)) (+ (* a a) (* b b)))
        ((and (>= a c) (>= b c)) (+ (* a a) (* c c)))
        )
  )


(define (a-plus-abs-b a b)
  ((if (> b 0) + -) a b))

(define (square x) (* x x))

(define (average x y)
  (/ (+ x y) 2))

(define (improve guess x)
  (average guess (/ x guess)))


(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x)
                 x)))

(define (sqrt x)
  (sqrt-iter 1.0 x))

(define (sqrt2 x)
  (define (improve guess)
    (define (average x y)
      (/ (+ x y) 2))
    (average guess (/ x guess)))
  (define (sqrt-iter last-guess next-guess)
    (define (good-enough?)
      (< (abs(/ (- last-guess next-guess)next-guess))
         0.001))
    (if (good-enough?)
        next-guess
        (sqrt-iter next-guess (improve next-guess))))
  (sqrt-iter 1.0 (improve 1.0)))


(define (improve3 x y) (/ (+ (/ x (* y y)) (* 2 y)) 3))

(define (cube-root x)
  (define (good-enough? guess x)
    (< (abs (-(* guess guess guess) x)) 0.001))
  (define (improve guess x)
    (/ (+ (/ x guess guess) guess guess) 3))
  (define (cube-root-iter guess x)
    (if (good-enough? guess x)
        guess
        (cube-root-iter (improve guess x) x)))
  (cube-root-iter 1.0 x))

(define (sqrt3 x)
  (define (good-enough? guess)
    (< (abs (- (square guess) x)) 0.001))
  (define (improve guess)
    (average guess (/ x guess)))
  (define (sqrt-iter guess)
    (if (good-enough? guess)
        guess
        (sqrt-iter (improve guess))))
  (sqrt-iter 1.0))

(define (factorical n)
  (if (= n 1)
      1
      (* n (factorical (- n 1)))))

(define (fact-iter product counter max-counter)
  (if (> counter max-counter)
      product
      (fact-iter (* counter product)
                 (+ counter 1)
                 max-counter)))


(define (factorical2 n)
  (fact-iter 1 1 n))

(define (inc a) (+ a 1))
(define (dec a) (- a 1))

(define (+ a b)
  (if (= a 0)
      b
      (inc (+ (dec a) b))))






