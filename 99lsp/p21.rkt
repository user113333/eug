#lang scheme

(define (insert-at a xs n)
  (cond
    ((null? xs) empty)
    ((= n 1) (cons a xs))
    ((= n 0) xs)
    ((< n 0) (reverse (insert-at a (reverse xs) (- n))))
    (else (cons (car xs) (insert-at a (cdr xs) (- n 1))))))
