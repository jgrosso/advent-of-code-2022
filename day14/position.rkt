#lang racket

(require "contract.rkt")

(struct Position (x y) #:transparent)

(define (string->Position text)
  (match-let ([(list x y) (string-split text ",")])
    (Position (string->number x) (string->number y))))

(define Nonnegative-Position/c
  (struct/c Position
            exact-nonnegative-integer?
            exact-nonnegative-integer?))

(provide/c
 (contract-out
  [struct Position ((x exact-integer?) (y exact-integer?))]
  [Nonnegative-Position/c contract?]
  [string->Position
   (-> string? Position?)]))
