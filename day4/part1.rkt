#lang racket

(define (within? sub sup)
  (and (>= (car sub) (car sup))
       (<= (cdr sub) (cdr sup))))

(define (read-string text)
  (read (open-input-string text)))

(define (parse-interval text)
  (match (string-split text "-")
    [(list left right)
     (cons (read-string left) (read-string right))]))

(define (parse-pair text)
  (match (string-split text ",")
    [(list left right)
     (cons (parse-interval left) (parse-interval right))]))

(let* ([input (port->string (open-input-file "input"))]
       [pairs (map parse-pair (string-split input "\n"))])
  (count (match-lambda [(cons x y)
                        (or (within? x y)
                            (within? y x))])
         pairs))
