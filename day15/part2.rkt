#lang racket

(require "beacon.rkt"
         "position.rkt")

(define minimum-possible-beacon-x 0)
(define maximum-possible-beacon-x 20) ; 4000000)

(define minimum-possible-beacon-y 0)
(define maximum-possible-beacon-y 20) ; 4000000)

(define input-string (port->string (open-input-file "example")))

(define sensors
  (map string->Sensor (string-split input-string "\n")))

(define visible-positions
  (apply set-union
         (map Sensor-visible-positions sensors)))

(define candidate-positions
  (for*/list ([x (inclusive-range minimum-possible-beacon-x
                                  maximum-possible-beacon-x)]
              [y (inclusive-range minimum-possible-beacon-y
                                  maximum-possible-beacon-y)]
              #:when (set-member? visible-positions (Position x y)))
    (Position x y)))

(first candidate-positions)
