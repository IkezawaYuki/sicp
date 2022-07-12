#lang racket

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

(fixed-point (lambda (x)(+ 1.0(/ 1.0 x))) 1.0)

(define tolerance2 0.0001)

(define (cont-frac n d k)
  (define (cf i)
    (if (= i k)
        (/ (n i)(d i))
        (/ (n i)(+ (d i)(cf (+ i 1))))))
  (cf 1))

(cont-frac-iter (lambda(i) 1.0)(lambda(i) 1.0) 12)

(define (cont-frac1 n d k)
  (define (cf res i)
    (if (= i 0)
        res
        (cf (/ (n i)(+ (d i) res))(- i 1))))
  (cf (/ (n k)(d k))(- k 1)))

(+ 2.0
   (cont-frac (lambda(i) 1.0)
              (lambda(i)(if (= (remainder i 3) 2)
                            (/ (+ i i 2.0) 3.0)
                            1.0))
              20))

(define (cont-frac3 n d k)
  (define (cf res i)
    (if (= i 0)
        res
        (cf (/ (n i)(+ (d i) res))(- i 1))))
  (cf (/ (n k)(d k))(- k 1)))

(define (tan-cf x k)
  (cont-frac (lambda (i)(if (= i 1) x (- (* x x))))
             (lambda (i)(+ i i -1.0)) k))

(tan-cf (/ 3.14159 4) 10)


(define (average-damp f)
  (lambda(x)(average x (f x))))

(define (sqrt x)
  (fixed-point (average-damp (lambda (y)(/ x y)))
               1.0))


(define (cube-root x)
  (fixed-point (average-damp (lambda (y)(/ x(square y))))
               1.0))

(define dx 0.0001)

(define (deriv g)
  (lambda (x)
    (/ (- (g (+ x dx)) (g x))
       dx)))

(define (cube x) (* x x x))

((derive cube) 5)

(define (newton-transform g)
  (lambda(x)
    (- x (/ (g x) ((deriv g) x)))))

(define (newtons-method g guess)
  (fixed-point (newton-transform g) guess))

(define (sqrt x)
  (newtons-method (lambda (y)(- (square y) x))
                  1.0))
