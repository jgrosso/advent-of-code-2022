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

(define (Position-manhattan-distance point1 point2)
  (+ (abs (- (Position-x point2) (Position-x point1)))
     (abs (- (Position-y point2) (Position-y point1)))))

(provide/c
 (contract-out
  [struct Position ((x exact-integer?) (y exact-integer?))]
  [Nonnegative-Position/c contract?]
  [Position-manhattan-distance
   (-> Position? Position? exact-nonnegative-integer?)]
  [string->Position
   (-> string? Position?)]))
