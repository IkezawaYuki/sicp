#lang racket

(define dx 0.00001)

(define (deriv g)
  (lambda(x)
    (/ (- (g (+ x dx)) (g x))
       dx)))

(define (newton-transform g)
  (lambda(x)
    (- x(/ (g x)((deriv g) x)))))

(define tolerance 0.00001)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(define (cubic a b c)
  (lambda(x)(+ (* (+ (* (+ x a) x) b) x) c)))

(newtons-method (cubic 3 3 1) 1)

(define (compose f g)
  (lambda(x) (f (g x))))

(define (square x) (* x x))

(define (repeated f n)
  (if (= n 0)
      (lambda (x) x)
      (compose f (repeated f(- n 1)))))

(define (oddp x)
  (= (remainder x 2) 1))

(define (repeated2 f n)
  (if (= n 0)
      (lambda (x) x)
      (compose f(repeated f(- n 1)))))

(define dx 0.1)

(define (smooth f)
  (lambda (x) (/ (+ (f x)(f (- x dx))(f (+ x dx))) 3)))

(define (n-fold-smooth f n)
  ((repeated smooth n) f))

(define (f x)
  (let ((ix (floor x)))
    (if (oddp ix)(+ (- ix x) 1)(-x ix))))

(do ((x 5(+ x 1)))((= x 25))
  (display (list (/ x 10)(f (/ x 10)))) (newline))

(define k 1)

(define (compose f g)
  (lambda (x)(f (g x))))

(define (repeated f n)
  (if (= n 0)
      (lambda(x) x)
      (compose f (repeated f (- n 1)))))


(define tolerance 0.00001)

(define (average x y)
  (/ (+ x y) 2))

(define (average-damp f)
  (lambda(x)(average x (f x))))


(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(define (n-th-root n x)
  (fixed-point ((repeated average-damp k)
                (lamdba (y) (/ x (empty y (- n 1))))) 1.0))

(define (iterative-improve test improve)
  (lambda (g)
    (define (iter g)
      (if (test g) g
          (iter (improve g))))
    (iter g)))


(define (sqrt x)
  (define (good-enough? g)
    (define (square x) (* x x))
    (< (abs (- (square g) x)) 0.0001))
  (define (improve g)
    (define (average x y)(/ (+ x y) 2))
    (average g (/ x g)))
  ((iterative-improve good-enough? improve) 1.0))


(define (fixed-point f)
  (define (close-enough? g)
    (< (abs (- g (f g))) 0.0001))
  ((iterative-improve close-enough? f) 1.0))




