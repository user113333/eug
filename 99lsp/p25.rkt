#lang scheme

(define (remove-at xs n)
  (cond
    ((null? xs) empty)
    ((= n 0) (cdr xs))
    (else (cons (car xs) (remove-at (cdr xs) (- n 1))))))

; returns: (random-element (other-elements))
(define (rs xs)
  (let ((n (inexact->exact (floor (* (length xs) (random))))))
    (list (list-ref xs n) (remove-at xs n))))

(define (rnd-permu xs)
  (if (null? xs)
      empty
      (let ((lp (rs xs)))
        (cons (first lp) (rnd-permu (second lp))))))