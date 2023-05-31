#lang scheme

; (flatten '(2 (3 ((4) 5))))
(define (flatten x)
  (cond
    ((null? x) null)
    ((number? (car x)) (cons (car x) (flatten (cdr x))))
    (else (flatten (append (car x) (cdr x))))))

; (flatten '(2 (3 ((4) 5))))
(define (my-flatten x)
  (cond
    ((null? x) null)
    ((not (list? (car x))) (cons (car x) (flatten (cdr x))))
    (else (flatten (append (car x) (cdr x))))))
