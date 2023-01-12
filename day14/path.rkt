#lang racket

(require "contract.rkt"
         "position.rkt"
         "utility.rkt")

(struct Line (start end))

(define (Line-horizontal? line)
  (= (Position-y (Line-start line))
     (Position-y (Line-end line))))

(define (Line-vertical? line)
  (= (Position-x (Line-start line))
     (Position-x (Line-end line))))

(define (Line-connected-to? line1 line2)
  (equal? (Line-end line1) (Line-start line2)))

(define Path-Segment/c
  (and/c Line?
         (or/c Line-horizontal? Line-vertical?)))

(define (Path-Segment-length path-segment)
  (let ([get-varying-coord
         (cond
           [(Line-horizontal? path-segment) Position-x]
           [(Line-vertical? path-segment) Position-y])])
    (add1 (abs (- (get-varying-coord (Line-start path-segment))
                  (get-varying-coord (Line-end path-segment)))))))

(define (Path-Segment-points path-segment)
  (match-let*-values
      ([(get-varying-coord get-fixed-coord make-Position)
        (cond
          [(Line-horizontal? path-segment)
           (values Position-x Position-y Position)]
          [(Line-vertical? path-segment)
           (values Position-y Position-x (flip Position))])]
       [((list varying-coord-start varying-coord-end))
        (sort (list (get-varying-coord (Line-start path-segment))
                    (get-varying-coord (Line-end path-segment)))
              <)]
       [(fixed-coord) (get-fixed-coord (Line-start path-segment))])
    (for/list ([varying-coord
                (range varying-coord-start (add1 varying-coord-end))])
      (make-Position varying-coord fixed-coord))))

(struct Path (waypoints))

(define (Path-segments path)
  (map (curry apply Line) (pairs (Path-waypoints path))))

(define (string->Path text)
  (Path (map string->Position (string-split text " -> "))))

(provide/c
 (contract-out
  [struct Line ((start Position?) (end Position?))]
  [Line-horizontal?
   (-> Line? boolean?)]
  [Line-vertical?
   (-> Line? boolean?)]
  [Path-Segment/c contract?]
  [Path-Segment-length
   (-> Line? exact-positive-integer?)]
  [Path-Segment-points
   (->i ([path-segment Path-Segment/c])
        [result (path-segment)
                (and/c (listof Position?)
                       (property/c length
                                   (=/c (Path-Segment-length path-segment))))])]
  [struct Path ((waypoints (listof Position?)))]
  [Path-segments
   (-> Path?
       (and/c (listof Path-Segment/c)
              (property/c pairs (listof (curry apply Line-connected-to?)))))]
  [string->Path
   (-> string? Path?)]))
