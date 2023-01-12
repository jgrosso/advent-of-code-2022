#lang racket

(require "beacon.rkt"
         "position.rkt")

(define minimum-possible-beacon-x 0)
(define maximum-possible-beacon-x 4000000)

(define minimum-possible-beacon-y 0)
(define maximum-possible-beacon-y 4000000)

(define input-string (port->string (open-input-file "input")))

(define sensors
  (map string->Sensor (string-split input-string "\n")))

(define (first-empty-position-in-row y [x minimum-possible-beacon-x])
  (define (jump-past sensor position)
    (let* ([y-distance
            (abs (- (Position-y position)
                    (Position-y (Sensor-position sensor))))]
           [x-delta (- (Sensor-range sensor) y-distance)])
      (+ (Position-x (Sensor-position sensor)) x-delta)))

  (and (<= x maximum-possible-beacon-x)
       (let* ([position (Position x y)]
              [sensor? (findf (λ (sensor) (Sensor-can-see? sensor position))
                              sensors)])
         (if sensor?
             (first-empty-position-in-row y (max (jump-past sensor? position)
                                                 (add1 x)))
             position))))

(Beacon-tuning-frequency (ormap first-empty-position-in-row
                                (inclusive-range minimum-possible-beacon-y
                                                 maximum-possible-beacon-y)))
