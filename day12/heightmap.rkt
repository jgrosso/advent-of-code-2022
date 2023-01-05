#lang racket

(require data/queue
         "grid.rkt"
         "utility.rkt")

(define min-elevation 0)

(define max-elevation 25)

(define elevation/c
  (and/c exact-nonnegative-integer?
         (>=/c min-elevation)
         (<=/c max-elevation)))

(struct Heightmap (elevations origin destination))

(define (Heightmap-height heightmap)
  (Grid-height (Heightmap-elevations heightmap)))

(define (Heightmap-width heightmap)
  (Grid-width (Heightmap-elevations heightmap)))

(define (Heightmap-inside? heightmap position)
  (Grid-inside? (Heightmap-elevations heightmap) position))

(define (Heightmap-elevation-at heightmap position)
  (Grid-at (Heightmap-elevations heightmap) position))

(define (Heightmap-neighbors heightmap position)
  (let ([right
         (struct-copy Position position
                      [x (add1 (Position-x position))])]
        [left
         (struct-copy Position position
                      [x (sub1 (Position-x position))])]
        [top
         (struct-copy Position position
                      [y (sub1 (Position-y position))])]
        [bottom
         (struct-copy Position position
                      [y (add1 (Position-y position))])])
    (for/list ([neighbor (list right left top bottom)]
               #:when (Heightmap-inside? heightmap neighbor))
      neighbor)))

(define (Heightmap-can-step? heightmap from to)
  (<= (Heightmap-elevation-at heightmap to)
      (+ (Heightmap-elevation-at heightmap from) 1)))

(define (queue-singleton value)
  (let ([queue (make-queue)])
    (enqueue! queue value)
    queue))

(define (Heightmap-shortest-distances heightmap)
  (do ([shortest-distances
        (make-Grid (Heightmap-width heightmap)
                   (Heightmap-height heightmap)
                   +inf.0)]
       [worklist (queue-singleton (cons -1 (Heightmap-destination heightmap)))])
      ((queue-empty? worklist)
       shortest-distances)
    (match-let* ([(cons distance destination) (dequeue! worklist)]
                 [old-distance (Grid-at shortest-distances destination)]
                 [distance (add1 distance)])
      (when (< distance old-distance)
        (set! shortest-distances
              (Grid-set-at shortest-distances destination distance))
        (for ([neighbor (Heightmap-neighbors heightmap destination)]
              #:when (Heightmap-can-step? heightmap neighbor destination))
          (enqueue! worklist (cons distance neighbor)))))))

(define (Heightmap-shortest-distance heightmap)
  (let ([shortest-distances (Heightmap-shortest-distances heightmap)])
    (Grid-at shortest-distances (Heightmap-origin heightmap))))

(define Heightmap/c
  (and/c Heightmap?
         (property/c Heightmap-elevations (Grid/c elevation/c))
         (λ (heightmap) (= (Heightmap-elevation-at heightmap
                                                   (Heightmap-destination heightmap))
                           max-elevation))))

(define (inside-Heightmap/c heightmap)
  (λ (position) (Heightmap-inside? heightmap position)))

(define (string->Heightmap text)
  (let* ([origin #f]
         [destination #f]
         [elevations
          (for/list ([line (enumerate (lines text))])
            (for/list ([char (enumerate (string->list (cdr line)))])
              (let ([position (Position (car char) (car line))])
                (match (cdr char)
                  [#\S
                   (set! origin position)
                   min-elevation]
                  [#\E
                   (set! destination position)
                   max-elevation]
                  [char
                   (- (char->integer char)
                      (char->integer #\a))]))))])
    (Heightmap (Grid elevations) origin destination)))

(define (elevation->string elevation)
  (string (integer->char (+ elevation (char->integer #\a)))))

(define (Heightmap->string heightmap)
  (Grid->string (Heightmap-elevations heightmap) elevation->string))

(define (Heightmap-position/c heightmap)
  (and/c Position?
         (inside-Heightmap/c heightmap)))

(define (Heightmap-filter heightmap proc?)
  (Grid-fold (Grid-enumerate (Heightmap-elevations heightmap))
             (match-lambda** [((cons position elevation) acc)
                              (if (proc? elevation)
                                  (cons position acc)
                                  acc)])
             (list)))

(provide
 (contract-out
  [elevation/c contract?]
  [min-elevation (and/c elevation/c (<=/c max-elevation))]
  [max-elevation (and/c elevation/c (>=/c min-elevation))]
  [Heightmap/c contract?]
  [Heightmap
   (->i ([elevations (Grid/c elevation/c)]
         [start (elevations) (Grid-position/c elevations)]
         [destination (elevations) (Grid-position/c elevations)])
        [result (elevations) (and/c Heightmap/c
                                    (property/c Heightmap-width
                                                (=/c (Grid-width elevations)))
                                    (property/c Heightmap-height
                                                (=/c (Grid-height elevations))))])]
  [Heightmap-origin
   (->i ([heightmap Heightmap?])
        [result (heightmap)
                (and/c Position?
                       (inside-Heightmap/c heightmap)
                       (property/c (curry Heightmap-elevation-at heightmap)
                                   (=/c min-elevation)))])]
  [Heightmap-destination
   (->i ([heightmap Heightmap?])
        [result (heightmap)
                (and/c Position?
                       (inside-Heightmap/c heightmap)
                       (property/c (curry Heightmap-elevation-at heightmap)
                                   (=/c max-elevation)))])]
  [Heightmap-height
   (-> Heightmap? exact-nonnegative-integer?)]
  [Heightmap-width
   (-> Heightmap? exact-nonnegative-integer?)]
  [Heightmap-inside?
   (-> Heightmap/c Position? boolean?)]
  [inside-Heightmap/c
   (-> Heightmap/c contract?)]
  [Heightmap-position/c
   (-> Heightmap/c contract?)]
  [Heightmap-filter
   (->i ([heightmap Heightmap/c]
         [proc? (-> elevation/c boolean?)])
        [result (heightmap proc?)
                (listof (and/c (Heightmap-position/c heightmap)
                               (property/c (curry Heightmap-elevation-at heightmap) proc?)))])]
  [Heightmap-elevation-at
   (->i ([heightmap Heightmap/c]
         [position (heightmap) (Heightmap-position/c heightmap)])
        [result elevation/c])]
  [Heightmap-neighbors
   (->i ([heightmap Heightmap/c]
         [position (heightmap) (Heightmap-position/c heightmap)])
        [result (heightmap) (and/c (listof (Heightmap-position/c heightmap))
                                   (property/c length (<=/c 4)))])]
  [Heightmap-can-step?
   (->i ([heightmap Heightmap/c]
         [from (heightmap) (Heightmap-position/c heightmap)]
         [to (heightmap) (Heightmap-position/c heightmap)])
        [result boolean?])]
  [Heightmap-shortest-distance
   (->i ([heightmap Heightmap/c])
        [result (heightmap)
                (or/c (=/c +inf.0)
                      (and/c exact-nonnegative-integer?
                             (<=/c (* (Heightmap-width heightmap)
                                      (Heightmap-height heightmap)))))])]
  [Heightmap-shortest-distances
   (->i ([heightmap Heightmap/c])
        [result (heightmap)
                (Grid/c (or/c (=/c +inf.0)
                              (and/c exact-nonnegative-integer?
                                     (<=/c (* (Heightmap-width heightmap)
                                              (Heightmap-height heightmap))))))])]
  [string->Heightmap
   (-> string? Heightmap/c)]
  [elevation->string
   (-> elevation/c string?)]
  [Heightmap->string
   (-> Heightmap/c string?)]))
