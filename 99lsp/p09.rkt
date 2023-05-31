#lang scheme

(define (pack_car xs)
  (cond
    ((null? (cdr xs)) (list (car xs)))
    ((eq? (car xs) (cadr xs)) (cons (car xs) (pack_car (cdr xs))))
    (else (list (car xs)))))

(define (pack_cdr xs)
  (cond
    ((null? (cdr xs)) null)
    ((eq? (car xs) (cadr xs)) (pack_cdr (cdr xs)))
    (else (cdr xs))))

; (pack '(1 1 1 1 2 3 3 1 1 4 5 5 5 5))
(define (pack xs)
  (if (null? xs)
      empty
      (cons (pack_car xs) (pack (pack_cdr xs)))))
