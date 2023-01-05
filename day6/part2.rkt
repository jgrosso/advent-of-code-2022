#lang racket

(define (run xs start n)
  (for/list ([i (range start (+ start n))])
    (list-ref xs i)))

(define (runs xs n)
  (for/list ([i (range 0 (+ 1 (- (length xs) n)))])
    (run xs i n)))

(define (start-marker? run)
  (not (check-duplicates run)))

(let* ([start-marker-length
        14]
       [buffer
        (string->list (port->string (open-input-file "input")))]
       [runs
        (runs buffer start-marker-length)])
  (+ (index-where runs start-marker?) start-marker-length))
