#lang scheme

(define (split xs n)
  (define (loop n a b)
    (if (or (null? b) (= n 0))
        (cons (reverse a) (list b))
        (loop (- n 1) (cons (car b) a) (cdr b))))
  (loop n empty xs))

; (rotate '(a b c d e f g h) 3)
; (D E F G H A B C)
; (rotate '(a b c d e f g h) -2)
; (G H A B C D E F)
(define (rotate xs n)
  (let ((sl (split xs (modulo n (length xs)))))
    (append (cadr sl) (car sl))))