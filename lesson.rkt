#lang racket

(define (square-list items)
  (if (null? items)
      '()
      (cons (square (car items)) (square-list (cdr items)))))

(define (square-lists items)
  (map (lambda (x) (square x)) items))

(define (square-list3 items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons (square (car things))
                    answer))))
  (iter items nil))

(define (for-each proc list)
  (if (null? list) '()
      (begin (proc (car list))
             (for-each proc (cdr list)))))
