#lang racket

(cons (list 1 2) (list 3 4))

(define x (cons (list 1 2) (list 3 4)))

(length x)

(define (count-leaves x)
  (cond ((null? x) 0)
        ((not (pair? x)) 1)
        (else (+ (count-leaves (car x))
                 (count-leaves (cdr x))))))

(count-leaves x)

(define (deep-reverse l)
  (if (pair? l)
      (append (deep-reverse (cdr l))
              (list (deep-reverse (car l))))
      l))

(define (fringe l)
  (if (pair? l)
      (if (pair? (car l))
          (append (fringe (car l)) (fringe (cdr l)))
          (cons (car l) (fringe (cdr l))))
      l))

(define (make-mobile left right)
  (list left right))

(define (left-branch mobile) (car mobile))
(define (right-branch mobile) (cadr mobile))

(define (branch-length branch) (car branch))
(define (branch-structure branch) (cadr branch))

(define (total-weight mobile)
  (if (not (pair? mobile)) mobile
      (let ((left-b (left-branch mobile))
            (right-b (right-branch mobile)))
        (let ((left-s (branch-structure left-b))
              (right-s (branch-structure right-b)))
          (+ (total-weight left-s)
             (total-weight right-s))))))

(define (balanced? mobile)
  (if (not (pair? mobile))
      (let ((left-b (left-branch mobile))
            (right-b (right-branch mobile)))
        (let ((left-s (branch-structure left-b))
              (right-s (branch-structure right-b)))
          (and
           (= (* (branch-length left-b)
                 (total-weight left-s))
              (* (branch-length right-b)
                 (total-weight right-s)))
           (balanced? left-s) (balanced? right-s))))))


