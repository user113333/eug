#lang scheme

(define (rnd-select xs n)
  (define (rs xs)
    (list-ref xs (inexact->exact (floor (* (length xs) (random))))))
  (cond
    ((null? xs) empty)
    ((= n 0) empty)
    (else (cons (rs xs) (rnd-select xs (- n 1))))))