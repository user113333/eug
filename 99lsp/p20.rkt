#lang scheme

; (remove-at '(a b c d) 2)
; (A C D)
; (remove-at '(a b c d) -2)
; (a b d)

(define (remove-at xs n)
  (cond
    ((null? xs) empty)
    ((= n 1) (cdr xs))
    ((= n 0) xs)
    ((< n 0) (reverse (remove-at (reverse xs) (- n))))
    (else (cons (car xs) (remove-at (cdr xs) (- n 1))))))