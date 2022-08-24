#lang racket

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (map p sequence)
  (accumulate (lambda (x y)
                (cons (p x) y)) '() sequence))

(map (lambda (x) (* x x)) '(0 1 2 3))

(define (append seq1 seq2)
  (accumulate cons seq2 seq1))


(define (length sequence)
  (accumulate (lambda (x y) (+ y 1)) 0 sequence))

(length '(a b c d))

(define (horner-eval x coefficient-sequence)
  (accumulate
   (lambda (this-coeff higer-terms)
     (+ (* higer-terms x) this-coeff))
   0
   coefficient-sequence))

(horner-eval 2 (list 1 3 0 5 0 1))

(define (count-leaves t)
  (accumulate + 0
              (map (lambda (x) (if (pair? x) (count-leaves x) 1)) t)))

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      '()
      (cons (accumulate op init (map (car seqs))
                        (accumulate-n op init (map cdr seqs))))))

(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(define (matrix-*-vector m v)
  (map (lambda (x) (dot-product v x)) m))

(define (transpose mat)
  (accumulate-n cons '() mat))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (x)
           (map (lambda (y)(dot-product x y)) cols)) m)))

(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))
