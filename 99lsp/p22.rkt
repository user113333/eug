#lang scheme

(define (range min max)
  (if (> min max)
      empty
      (cons min (range (+ min 1) max))))