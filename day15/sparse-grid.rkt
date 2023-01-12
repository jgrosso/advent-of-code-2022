#lang racket

(require "contract.rkt"
         "grid.rkt"
         "position.rkt")

(struct Sparse-Grid (unfilled-value filled-positions))

(define (empty-Sparse-Grid unfilled-value)
  (Sparse-Grid (make-hash) unfilled-value))

(define (Sparse-Grid-height sparse-grid)
  (add1 (apply max (map Position-y
                        (Sparse-Grid-filled-positions sparse-grid)))))

(define (Sparse-Grid-width sparse-grid)
  (add1 (apply max (map Position-x
                        (Sparse-Grid-filled-positions sparse-grid)))))

(define (Sparse-Grid-area sparse-grid)
  (* (Sparse-Grid-width sparse-grid)
     (Sparse-Grid-height sparse-grid)))

(define (Sparse-Grid->Grid sparse-grid)
  (matrix->Grid (for/list ([y (range 0 (Sparse-Grid-height sparse-grid))])
                  (for/list ([x (range 0 (Sparse-Grid-width sparse-grid))])
                    (Sparse-Grid-at (Position x y))))))

(define (Sparse-Grid->string sparse-grid . args)
  (apply Grid->string (Sparse-Grid->Grid sparse-grid) args))

(define (Sparse-Grid-inside? sparse-grid position)
  (and (<= 0 (Position-x position) (sub1 (Sparse-Grid-width sparse-grid)))
       (<= 0 (Position-y position) (sub1 (Sparse-Grid-height sparse-grid)))))

(define (Sparse-Grid-at sparse-grid position)
  (hash-ref (Sparse-Grid-filled-positions sparse-grid)
            position
            (λ () (Sparse-Grid-unfilled-value sparse-grid))))

(define (Sparse-Grid-map proc . sparse-grids)
  (let*
      ([unfilled-value
        (apply proc (map Sparse-Grid-unfilled-value sparse-grids))]
       [filled-keys
        (append-map hash-keys sparse-grids)]
       [filled-positions
        (map (λ (key)
               (apply proc
                      (map (λ (sparse-grid) (Sparse-Grid-at sparse-grid key))
                           sparse-grids)))
             filled-keys)])
    (Sparse-Grid unfilled-value filled-positions)))

(define (Sparse-Grid-any sparse-grid proc)
  (or (proc (Sparse-Grid-unfilled-value sparse-grid))
      (ormap proc (hash-values (Sparse-Grid-filled-positions sparse-grid)))))

(define (Sparse-Grid-member? sparse-grid x)
  (Sparse-Grid-any sparse-grid (λ (y) (equal? x y))))

(define (Sparse-Grid-merge sparse-grid1 sparse-grid2 choose)
  (Sparse-Grid-map (λ (x y) (if (choose x y) x y))
                   sparse-grid1
                   sparse-grid2))

(define (Sparse-Grid/c elem/c)
  (struct/c Sparse-Grid
            elem/c
            (hash/c Sparse-Grid-Position/c elem/c)))

(define (Sparse-Grid-member/c sparse-grid)
  (λ (x) (Sparse-Grid-member? sparse-grid x)))

(define (inside-Sparse-Grid/c sparse-grid)
  (λ (position) (Sparse-Grid-inside? sparse-grid position)))

(define (Sparse-Grid-Position/c sparse-grid)
  (and/c Position?
         (inside-Sparse-Grid/c sparse-grid)))

(define (Sparse-Grid-set-at sparse-grid position new-value)
  (struct-copy Sparse-Grid sparse-grid
               [filled-positions
                (hash-set (Sparse-Grid-filled-positions sparse-grid)
                          position new-value)]))

(provide/c
 (contract-out
  [Sparse-Grid/c
   (-> contract? contract?)]
  [Sparse-Grid
   (-> any/c (hash/c Sparse-Grid-Position/c any/c) (Sparse-Grid/c any/c))]
  [empty-Sparse-Grid
   (-> any/c (Sparse-Grid/c any/c))]
  [Sparse-Grid-member/c
   (-> (Sparse-Grid/c any/c) contract?)]
  [Sparse-Grid-Position/c
   (-> (Sparse-Grid/c any/c) contract?)]
  [Sparse-Grid->string
   (->i ([sparse-grid (Sparse-Grid/c any/c)])
        #:rest (listof any/c)
        [result string?])]
  [inside-Sparse-Grid/c
   (-> (Sparse-Grid/c any/c) contract?)]
  [Sparse-Grid-height
   (-> (Sparse-Grid/c any/c) exact-nonnegative-integer?)]
  [Sparse-Grid-width
   (-> (Sparse-Grid/c any/c) exact-nonnegative-integer?)]
  [Sparse-Grid-area
   (-> (Sparse-Grid/c any/c) exact-nonnegative-integer?)]
  [Sparse-Grid-inside?
   (-> (Sparse-Grid/c any/c) Position? boolean?)]
  [Sparse-Grid-at
   (->i ([sparse-grid (Sparse-Grid/c any/c)]
         [position (sparse-grid) (Sparse-Grid-Position/c sparse-grid)])
        [result any/c])]
  [Sparse-Grid-member?
   (-> (Sparse-Grid/c any/c) any/c boolean?)]
  [Sparse-Grid-map
   (->* ((-> any/c any/c))
        #:rest (listof (Sparse-Grid/c any/c))
        (Sparse-Grid/c any/c))]
  [Sparse-Grid-any
   (-> (Sparse-Grid/c any/c) (-> any/c boolean?) boolean?)]
  [Sparse-Grid-set-at
   (->i ([sparse-grid (Sparse-Grid/c any/c)]
         [position (sparse-grid) (Sparse-Grid-Position/c sparse-grid)]
         [new-value any/c])
        [result (position new-value)
                (and/c (Sparse-Grid/c any/c)
                       (λ (sparse-grid)
                         (equal? (Sparse-Grid-at grid position) new-value)))])]
  [Sparse-Grid-merge
   (->i ([sparse-grid1 (Sparse-Grid/c any/c)]
         [sparse-grid2 (Sparse-Grid/c any/c)]
         [choose (-> any/c any/c boolean?)])
        [result (sparse-grid1 sparse-grid2)
                (Sparse-Grid/c (or/c (Sparse-Grid-member/c sparse-grid1)
                                     (Sparse-Grid-member/c sparse-grid2)))])]))
