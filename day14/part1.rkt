#lang racket

(require "cave.rkt"
         "path.rkt"
         "position.rkt"
         "utility.rkt")

(define input-string (port->string (open-input-file "input")))

(define rock-paths (map string->Path (lines input-string)))

(define initial-cave (make-Cave-with-abyss rock-paths (Position 500 0)))

(define final-cave (fixed-point-from Cave-drop-sand initial-cave))

(display (Cave-sand-amount final-cave))
