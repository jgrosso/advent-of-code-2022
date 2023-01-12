#lang racket

(require "contract.rkt"
         "grid.rkt"
         "path.rkt"
         "position.rkt"
         "utility.rkt")

(define Cave-Material/c (or/c 'air 'rock 'sand))

(define (Cave-Material->string cave-material)
  (match cave-material
    ['air "."]
    ['rock "#"]
    ['sand "+"]))

(struct Cave (grid sand-spawn-position))

(define (make-Cave-with-abyss rock-paths sand-spawn-position)
  (let* ([hull-points
          (cons sand-spawn-position (append-map Path-waypoints rock-paths))]
         [cave-top 0]
         [cave-bottom (apply max (map Position-y hull-points))]
         [cave-left (apply min (map Position-x hull-points))]
         [cave-right (apply max (map Position-x hull-points))]
         [cave-height (add1 (- cave-bottom cave-top))]
         [cave-width (add1 (- cave-right cave-left))]
         [grid
          (for*/fold ([grid (make-Grid cave-width cave-height 'air)])
                     ([rock-path rock-paths]
                      [rock-path-segment (Path-segments rock-path)]
                      [rock-position (Path-Segment-points rock-path-segment)])
            (let ([rock-position
                   (struct-copy Position rock-position
                                [x (- (Position-x rock-position) cave-left)])])
              (Grid-set-at grid rock-position 'rock)))]
         [sand-spawn-position
          (Position (- (Position-x sand-spawn-position) cave-left)
                    (- (Position-y sand-spawn-position) cave-top))])
    (Cave grid sand-spawn-position)))

(define (make-Cave-with-floor rock-paths sand-spawn-position)
  (let* ([hull-points (append-map Path-waypoints rock-paths)]
         [lowest-rock-y (apply max (map Position-y hull-points))]
         [floor-y (+ 2 lowest-rock-y)]
         [floor-x-start (- (Position-x sand-spawn-position) floor-y)]
         [floor-x-end (+ (Position-x sand-spawn-position) floor-y)]
         [floor-path (Path (list (Position floor-x-start floor-y)
                                 (Position floor-x-end floor-y)))])
    (make-Cave-with-abyss (cons floor-path rock-paths) sand-spawn-position)))

(define (Cave->string cave)
  (Grid->string (Cave-grid cave) Cave-Material->string))

(define (Cave-material-at cave position)
  (Grid-at (Cave-grid cave) position))

(define (Cave-next-sand-position cave sand-position)
  (and sand-position
       (let ([below
              (struct-copy Position sand-position
                           [y (add1 (Position-y sand-position))])]
             [below-left
              (Position (sub1 (Position-x sand-position))
                        (add1 (Position-y sand-position)))]
             [below-right
              (Position (add1 (Position-x sand-position))
                        (add1 (Position-y sand-position)))])
         (cond
           [(not (Grid-inside? (Cave-grid cave) below)) #f]
           [(equal? (Cave-material-at cave below) 'air)
            below]
           [(not (Grid-inside? (Cave-grid cave) below-left)) #f]
           [(equal? (Cave-material-at cave below-left) 'air)
            below-left]
           [(not (Grid-inside? (Cave-grid cave) below-right)) #f]
           [(equal? (Cave-material-at cave below-right) 'air)
            below-right]
           [else sand-position]))))

(define (Cave-can-spawn-sand? cave)
  (equal? (Cave-material-at cave (Cave-sand-spawn-position cave)) 'air))

(define (Cave-drop-sand cave)
  (if (Cave-can-spawn-sand? cave)
      (let ([final-sand-position
             (fixed-point-from (curry Cave-next-sand-position cave)
                               (Cave-sand-spawn-position cave))])
        (if final-sand-position
            (struct-copy Cave cave
                         [grid (Grid-set-at (Cave-grid cave)
                                            final-sand-position 'sand)])
            cave))
      cave))

(define (Cave-Position/c cave)
  (Grid-Position/c (Cave-grid cave)))

(define (Cave-sand-amount cave)
  (Grid-sum (Grid-map (λ (material) (if (equal? material 'sand) 1 0))
                      (Cave-grid cave))))

(provide/c
 (contract-out
  [make-Cave-with-abyss
   (-> (listof Path?)
       Nonnegative-Position/c
       (and/c Cave?
              (property/c Cave-sand-amount zero?)))]
  [make-Cave-with-floor
   (-> (listof Path?)
       Nonnegative-Position/c
       (and/c Cave?
              (property/c Cave-sand-amount zero?)))]
  [Cave-sand-spawn-position
   (->i ([cave Cave?])
        [result (cave) (Cave-Position/c cave)])]
  [Cave-Position/c
   (-> Cave? contract?)]
  [Cave-Material/c contract?]
  [Cave-Material->string
   (-> Cave-Material/c string?)]
  [Cave->string
   (-> Cave? string?)]
  [Cave-material-at
   (->i ([cave Cave?]
         [position (cave) (Cave-Position/c cave)])
        [result Cave-Material/c])]
  [Cave-drop-sand
   (-> Cave? Cave?)]
  [Cave-sand-amount
   (->i ([cave Cave?])
        [result (cave)
                (and/c exact-nonnegative-integer?
                       (<=/c (Grid-area (Cave-grid cave))))])]))
