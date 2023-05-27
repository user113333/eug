#lang scheme

(define (my-last x)
  (if (null? x)
      null
      (if (null? (cdr x))
          (car x)
          (my-last (cdr x)))))
