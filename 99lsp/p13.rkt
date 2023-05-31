#lang scheme

(define (encode-direct+ xs n)
  (cond
    ((null? (cdr xs)) (list (list (+ n 1) (car xs))))
    ((eq? (car xs) (cadr xs)) (encode-direct+ (cdr xs) (+ n 1)))
    ((eq? n 0) (cons (car xs) (encode-direct+ (cdr xs) 0)))
    (else (cons (list (+ n 1) (car xs)) (encode-direct+ (cdr xs) 0)))))

(define (encode-direct xs)
  (encode-direct+ xs 0))