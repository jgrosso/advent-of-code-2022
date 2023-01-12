#lang racket

(require "packet.rkt"
         "utility.rkt")

(define divider-packets '(((2)) ((6))))

(define packets
  (append divider-packets
          (map string->packet
               (lines (port->string (open-input-file "input"))))))
(define ordered-packets (sort packets ordered-packets?))

(product (map add1 (indexes-where ordered-packets (λ (x) (member x divider-packets)))))
