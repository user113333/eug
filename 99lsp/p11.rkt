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

(define (pack xs)
  (if (null? xs)
      empty
      (cons (pack_car xs) (pack (pack_cdr xs)))))

; (encode-modified '(a a a a b c c a a d e e e e))
(define (encode-modified xs)
  (map
   (lambda (x)
     (if (eq? (length x) 1)
         (car x)
         (list (length x) (car x))))
   (pack xs)))
