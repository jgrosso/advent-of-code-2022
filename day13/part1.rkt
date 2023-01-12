#lang racket

(require "packet.rkt"
         "utility.rkt")

(define packet-pairs
  (chunk (map string->packet
              (lines (port->string (open-input-file "input"))))
         2))

(indexes-where packet-pairs
               (λ (pair) (apply ordered-packets? pair)))

(total (map add1
            (indexes-where packet-pairs
                           (λ (pair) (apply ordered-packets? pair)))))
