#lang scheme

; p22.rkt
(define (range min max)
  (if (> min max)
      empty
      (cons min (range (+ min 1) max))))

; p23.rkt
(define (rnd-select xs n)
  (define (rs xs)
    (list-ref xs (inexact->exact (floor (* (length xs) (random))))))
  (cond
    ((null? xs) empty)
    ((= n 0) empty)
    (else (cons (rs xs) (rnd-select xs (- n 1))))))

(define (lotto-select l m)
  (rnd-select (range 1 m) l))