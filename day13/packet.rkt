#lang racket

(define packet/c
  (flat-rec-contract packet
                     (listof packet)
                     number?))

(define (string->packet text)
  (read (open-input-string (string-replace text "," " "))))

(define (ordered-packets? left-packet right-packet)
  (match* (left-packet right-packet)
    [((? integer?) (? integer?))
     (< left-packet right-packet)]
    [((? integer?) (? list?))
     (ordered-packets? (list left-packet) right-packet)]
    [((? list?) (? integer?))
     (ordered-packets? left-packet (list right-packet))]
    [((== null) (== null)) #f]
    [((== null) (cons _ _)) #t]
    [((cons _ _) (== null)) #f]
    [((cons left-value left-packet) (cons right-value right-packet))
     (cond
       [(ordered-packets? left-value right-value) #t]
       [(ordered-packets? right-value left-value) #f]
       [else (ordered-packets? left-packet right-packet)])]))

(provide
 (contract-out
  [packet/c contract?]
  [string->packet
   (-> string? packet/c)]
  [ordered-packets?
   (-> packet/c packet/c boolean?)]))
