#lang scheme

(define (drop xs n)
  (define (helper xs n original)
    (cond
      ((null? xs) empty)
      ((= n 1) (helper (cdr xs) original original))
      (else (cons (car xs) (helper (cdr xs) (- n 1) original)))))
  (helper xs n n))