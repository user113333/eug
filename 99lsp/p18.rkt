#lang scheme

(define (slice xs start end)
  (cond
    ((or (<= end 0) (null? xs)) empty)
    ((<= start 1) (cons (car xs) (slice (cdr xs) (- start 1) (- end 1))))
    (else (slice (cdr xs) (- start 1) (- end 1)))))