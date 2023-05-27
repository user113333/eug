#lang scheme

(define (my-but-last x)
  (cond
    ((null? x) null)
    ((null? (cdr x)) null)
    ((null? (cdr (cdr x))) x)
    (true (my-but-last (cdr x)))))
