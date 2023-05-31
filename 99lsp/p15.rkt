#lang scheme

(define (replicate n e)
  (if (<= n 0)
      empty
      (cons e (replicate (- n 1) e))))

(define (repli xs n)
  (foldr (lambda (a b) (append (replicate n a) b)) empty xs))