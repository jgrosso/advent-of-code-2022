#lang racket

(require "beacon.rkt"
         "position.rkt")

(define y 2000000)

(define input-string (port->string (open-input-file "input")))

(define sensors (map string->Sensor (string-split input-string "\n")))

(define empty-positions
  (let* ([minimum-x-visible
          (apply min (map Sensor-minimum-x-visible sensors))]
         [maximum-x-visible
          (apply max (map Sensor-maximum-x-visible sensors))])
    (filter (λ (x) (not (Beacon-possible-at? sensors (Position x y))))
            (inclusive-range minimum-x-visible maximum-x-visible))))

(length empty-positions)
