#lang scheme

(define (split xs n)
  (define (drop n xs)
    (cond
      ((null? xs) empty)
      ((< n 1) (cons (car xs) (drop (- n 1) (cdr xs))))
      (else (drop (- n 1) (cdr xs)))))
  (define (take n xs)
    (cond
      ((null? xs) empty)
      ((= n 0) empty)
      (else (cons (car xs) (take (- n 1) (cdr xs))))))
  (cons (take n xs) (list (drop n xs))))

(define (split_optimized xs n)
  (define (loop n a b)
    (if (or (null? b) (= n 0))
        (cons (reverse a) (list b))
        (loop (- n 1) (cons (car b) a) (cdr b))))
  (loop n empty xs))