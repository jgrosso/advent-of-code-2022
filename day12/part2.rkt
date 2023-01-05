#lang racket

(require "grid.rkt"
         "heightmap.rkt")

(define heightmap (string->Heightmap (port->string (open-input-file "input"))))

(define origin-candidates (Heightmap-filter heightmap zero?))

(define shortest-distances (Heightmap-shortest-distances heightmap))

(apply min (map (λ (origin) (Grid-at shortest-distances origin))
                origin-candidates))
