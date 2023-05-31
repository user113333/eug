#lang scheme

(define (replicate n e)
  (if (<= n 0)
      empty
      (cons e (replicate (- n 1) e))))

(define (flatten xs)
  (cond
    ((null? xs) empty)
    ((list? (car xs)) (append (flatten (car xs)) (flatten (cdr xs))))
    (else (cons (car xs) (flatten (cdr xs))))))

; (decode '((4 a) b (2 c) (2 a) d (4 e)))
(define (decode xs)
  (flatten (map
   (lambda (x)
     (if (list? x)
         (replicate (car x) (cadr x))
         x))
   xs)))