#lang racket

(require "heightmap.rkt")

(define heightmap (string->Heightmap (port->string (open-input-file "input"))))

(Heightmap-shortest-distance heightmap)
