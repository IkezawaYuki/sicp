#lang racket

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(accumulate + 0 (list 1 2 3 4 5))

(accumulate * 1 (list 1 2 3 4 5))

(define (enumurate-interval low high)
  (if (> low high)
      null
      (cons low (enumurate-interval (+ low 1) high))))

(enumurate-interval 2 8)

(define (enumurate-tree tree)
  (cond ((null? tree) null)
        ((not (pair? tree)) (list tree))
        (else (append (enumurate-tree (car tree))
                      (enumurate-tree (cdr tree))))))

(enumurate-tree (list 1 (list 2 (list 3 4)) 5))


(define (even-fibs n)
  (accumulate cons
              null
              (filter even?
                      (map fib
                           (enumerate-inteval 0 n)))))