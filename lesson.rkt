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

(define (cont-frac n d k)
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

(define (cont-frac n d k)
  (define (cf res i)
    (if (= i 0)
        res
        (cf (/ (n i)(+ (d i) res))(- i 1))))
  (cf (/ (n k)(d k))(- k 1)))

(define (tan-cf x k)
  (cont-frac (lambda (i)(if (= i 1) x (- (* x x))))
             (lambda (i)(+ i i -1.0)) k))

(tan-cf (/ 3.14159 4) 10)





