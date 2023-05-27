#lang scheme

; (define (element-at x a)
;   (if (eq? a 1)
;       (car x)
;       (element-at (cdr x) (- a 1))))

(define (element-at x a)
  (cond
    ((null? x) null)
    ((eq? a 1) (car x))
    (else (element-at (cdr x) (- a 1)))))
