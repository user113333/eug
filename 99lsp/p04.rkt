#lang scheme

(define (number-of-elements x)
  (if (null? x)
      0
      (+ 1 (number-of-elements (cdr x)))))
