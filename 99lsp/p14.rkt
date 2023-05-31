#lang scheme

(define (dupli xs)
  (foldr (lambda (a b) (cons a (cons a b))) empty xs))