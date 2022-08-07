#lang racket


(define (square-tree tree)
  (cond ((null? tree) '())
        ((pair? tree) (cons (square-tree (car tree))
                            (square-tree (cdr tree))))
        (else (* tree tree))))


(square-tree (list 1
                   (list 2 (list 3 4) 5)
                   (list 6 7)))


(define (square-tree tree)
  (if (pair? tree)
      (map (lambda (x) (square-tree x)) tree)
      (* tree tree)))


(define (tree-map f tree)
  (cond ((null? tree) '())
        ((not (pair? tree)) (f tree))
        (else (cons (tree-map f (car tree))
                    (tree-map f (cdr tree))))))

