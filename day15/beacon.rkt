#lang racket

(require "contract.rkt"
         "position.rkt")

(struct Sensor (position closest-beacon))

(define (Sensor-range sensor)
  (Position-manhattan-distance (Sensor-position sensor)
                               (Sensor-closest-beacon sensor)))

(define (Sensor-minimum-x-visible sensor)
  (- (Sensor-range sensor)
     (Position-x (Sensor-position sensor))))

(define (Sensor-maximum-x-visible sensor)
  (+ (Sensor-range sensor)
     (Position-x (Sensor-position sensor))))

(define (Sensor-minimum-y-visible sensor)
  (- (Sensor-range sensor)
     (Position-y (Sensor-position sensor))))

(define (Sensor-maximum-y-visible sensor)
  (+ (Sensor-range sensor)
     (Position-y (Sensor-position sensor))))

(define (Sensor-can-see? sensor position)
  (<= (Position-manhattan-distance (Sensor-position sensor) position)
      (Sensor-range sensor)))

(define (string->Sensor text)
  (match-let*
      ([(list _ sensor-x sensor-y beacon-x beacon-y)
        (regexp-match #rx"Sensor at x=(-?[0-9]+), y=(-?[0-9]+): closest beacon is at x=(-?[0-9]+), y=(-?[0-9]+)" text)]
       [position
        (Position (string->number sensor-x)
                  (string->number sensor-y))]
       [closest-beacon
        (Position (string->number beacon-x)
                  (string->number beacon-y))])
    (Sensor position closest-beacon)))

(define Beacon/c
  Position?)

(define (Beacon-tuning-frequency beacon)
  (+ (* (Position-x beacon) 4000000)
     (Position-y beacon)))

(define (Beacon-possible-at? sensors position)
  (let ([visible?
         (ormap (λ (sensor) (Sensor-can-see? sensor position))
                sensors)]
        [beacon?
         (ormap (λ (sensor) (equal? position
                                    (Sensor-closest-beacon sensor)))
                sensors)])
    (or beacon? (not visible?))))

(define (Sensor-visible-positions sensor)
  (apply set
         (for*/list ([x (inclusive-range (Sensor-minimum-x-visible sensor)
                                         (Sensor-maximum-x-visible sensor))]
                     [y (inclusive-range (Sensor-minimum-y-visible sensor)
                                         (Sensor-maximum-y-visible sensor))])
           (Position x y))))

(provide/c
 (contract-out
  [Beacon/c
   contract?]
  [Beacon-tuning-frequency
   (-> Beacon/c exact-integer?)]
  [Beacon-possible-at?
   (-> (listof Sensor?) Position? boolean?)]
  [struct Sensor ((position Position?)
                  (closest-beacon Beacon/c))]
  [Sensor-range
   (-> Sensor? exact-nonnegative-integer?)]
  [Sensor-minimum-x-visible
   (->i ([sensor Sensor?])
        [result (sensor)
                (and/c exact-integer?
                       (<=/c (Sensor-maximum-x-visible sensor)))])]
  [Sensor-maximum-x-visible
   (->i ([sensor Sensor?])
        [result (sensor)
                (and/c exact-integer?
                       (>=/c (Sensor-minimum-x-visible sensor)))])]
  [Sensor-minimum-y-visible
   (->i ([sensor Sensor?])
        [result (sensor)
                (and/c exact-integer?
                       (<=/c (Sensor-maximum-y-visible sensor)))])]
  [Sensor-maximum-y-visible
   (->i ([sensor Sensor?])
        [result (sensor)
                (and/c exact-integer?
                       (>=/c (Sensor-minimum-y-visible sensor)))])]
  [Sensor-can-see?
   (-> Sensor? Position? boolean?)]
  [Sensor-visible-positions
   (->i ([sensor Sensor?])
        [result (sensor)
                (set/c (and/c Position?
                              (λ (position) (Sensor-can-see? sensor position))))])]
  [string->Sensor
   (-> string? Sensor?)]))
