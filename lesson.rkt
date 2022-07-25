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


(define (sub-interval x y)
  (make-interval (- (lower-bound x) (upper-bound y))
                 (- (upper-bound x) (lower-bound y))))

(define (make-interval a b) (cons a b))
(define (lower-bound x) (car x))
(define (upper-bound x) (cdr x))

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(define (sub-internal x y)
  (make-interval (- (lower-bound x) (lower-bound y))
                 (- (upper-bound x) (upper-bound y))))


(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))


(define (div-interval x y)
  (mul-interval x
                (make-interval (/ 1.0 (upper-bound y))
                               (/ 1.0 (lower-bound y)))))

(define (make-interval a b)(cons a b))
(define (upper-bound i) (cdr i))
(define (lower-bound i) (car i))

(define (make-center-width w)
  (make-interval (- c w) (+ c w)))

(define (percent i)
  (* (/ (- (upper-bound i)(lower-bound i)) (center i)) 100))

(define (center i)
  (/ (+ (upper-bound i) (lower-bound i)) 2))

(define one-through-four (list 1 2 3 4))






