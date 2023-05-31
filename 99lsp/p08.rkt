#lang scheme

(define (compress+ x last)
  (cond
    ((null? x) empty)
    ((eq? (car x) last) (compress+ (cdr x) last))
    (else (cons (car x) (compress+ (cdr x) (car x))))))

; (compress '(1 1 1 1 2 3 3 1 1 4 5 5 5 5))
(define (compress x)
  (compress+ x null))

(define (compress_simple xs)
  (cond
    ((null? (cdr xs)) xs)
    ((eq? (car xs) (cadr xs)) (compress_simple (cdr xs)))
    (else (cons (car xs) (compress_simple (cdr xs))))))
    