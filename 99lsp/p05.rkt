#lang scheme

; (define '(1 2 3 4) '())
(define (reverse+ a b)
  (if (null? a)
      b
      (reverse+ (cdr a) (cons (car a) b))))

(define (reverse x) (reverse+ x '()))

(define (reverse-simple x) (foldl cons '() x))
