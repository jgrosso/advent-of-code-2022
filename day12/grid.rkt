#lang racket

(require "utility.rkt")

(struct Position (x y) #:transparent)

(struct Grid (rows) #:transparent)

(define (make-Grid width height init)
  (Grid (repeat (repeat init width) height)))

(define (Grid-height grid)
  (length (Grid-rows grid)))

(define (Grid-width grid)
  (let ([rows (Grid-rows grid)])
    (if (null? rows)
        0
        (length (car rows)))))

(define (Grid->string grid [element->string ~a])
  (let* ([naive-rendering (Grid-map element->string grid)]
         [max-column-width (Grid-fold (Grid-map string-length naive-rendering)
                                      max 0)]
         [aligned-rendering (Grid-map (λ (s) (~a s #:width max-column-width))
                                      naive-rendering)])
    (string-join (map (λ (x) (string-join x "")) (Grid-rows aligned-rendering))
                 "\n")))

(define (Grid-inside? grid position)
  (and (<= 0 (Position-x position) (sub1 (Grid-width grid)))
       (<= 0 (Position-y position) (sub1 (Grid-height grid)))))

(define (Grid-at grid position)
  (let ([row (list-ref (Grid-rows grid) (Position-y position))])
    (list-ref row (Position-x position))))

(define (Grid-map proc . grids)
  (Grid (apply (curry map (curry map proc))
               (map Grid-rows grids))))

(define (Grid-fold grid proc init)
  (foldl (λ (row acc) (foldl proc acc row))
         init
         (Grid-rows grid)))

(define (Grid-indices grid)
  (Grid (for/list ([row-index (range 0 (Grid-height grid))])
          (for/list ([column-index (range 0 (Grid-width grid))])
            (Position column-index row-index)))))

(define (Grid-enumerate grid)
  (Grid-map cons (Grid-indices grid) grid))

(define (Grid-and grid)
  (Grid-fold grid (λ (x acc) (and x acc)) true))

(define (Grid-or grid)
  (Grid-fold grid (λ (x acc) (or x acc)) false))

(define (Grid-all grid proc)
  (Grid-and (Grid-map proc grid)))

(define (Grid-any grid proc)
  (Grid-or (Grid-map proc grid)))

(define (Grid-member? grid x)
  (Grid-any grid (λ (y) (equal? x y))))

(define (Grid-merge grid1 grid2 choose)
  (Grid-map (λ (x y) (if (choose x y) x y))
            grid1
            grid2))

(define (Grid/c elem/c)
  (and/c Grid?
         (λ (grid) (Grid-all grid (flat-contract-predicate elem/c)))))

(define (Grid-member/c grid)
  (λ (x) (Grid-member? grid x)))

(define (inside-Grid/c grid)
  (λ (position) (Grid-inside? grid position)))

(define (matrix? rows)
  (if (null? rows)
      #t
      (let ([dimension (length (car rows))])
        (andmap (λ (row) (= (length row) dimension))
                rows))))

(define matrix/c
  (and/c (listof (listof any/c))
         matrix?))

(define (Grid-position/c grid)
  (and/c Position?
         (inside-Grid/c grid)))

(define (Grid-set-at grid position new-value)
  (Grid (list-update (Grid-rows grid)
                     (Position-y position)
                     (λ (row) (list-set row (Position-x position) new-value)))))

(provide
 (contract-out
  [struct Position
    ((x exact-integer?)
     (y exact-integer?))]
  [matrix?
   (-> (listof (listof any/c)) boolean?)]
  [matrix/c contract?]
  [Grid/c
   (-> flat-contract? contract?)]
  [Grid
   (-> matrix/c (Grid/c any/c))]
  [Grid-member/c
   (-> (Grid/c any/c) contract?)]
  [Grid-position/c
   (-> (Grid/c any/c) contract?)]
  [Grid->string
   (->i ([grid (Grid/c any/c)])
        ([element->string (-> any/c string?)])
        [result string?])]
  [inside-Grid/c
   (-> (Grid/c any/c) contract?)]
  [make-Grid
   (->i ([width exact-nonnegative-integer?]
         [height exact-nonnegative-integer?]
         [init any/c])
        [result (width height init)
                (and/c (property/c Grid-width (=/c width))
                       (property/c Grid-height (=/c height))
                       (Grid/c (equal/c init)))])]
  [Grid-height
   (-> (Grid/c any/c) exact-nonnegative-integer?)]
  [Grid-width
   (-> (Grid/c any/c) exact-nonnegative-integer?)]
  [Grid-inside?
   (-> (Grid/c any/c) Position? boolean?)]
  [Grid-at
   (->i ([grid (Grid/c any/c)]
         [position (grid) (Grid-position/c grid)])
        [result any/c])]
  [Grid-member?
   (-> (Grid/c any/c) any/c boolean?)]
  [Grid-map
   (->* ((-> any/c any/c)) #:rest (Grid/c any/c) (Grid/c any/c))]
  [Grid-fold
   (->i ([grid (Grid/c any/c)]
         [proc (grid) (-> (Grid-member/c grid) any/c any/c)]
         [init any/c])
        [result any/c])]
  [Grid-and
   (-> (Grid/c boolean?) boolean?)]
  [Grid-or
   (-> (Grid/c boolean?) boolean?)]
  [Grid-all
   (-> (Grid/c any/c) (-> any/c boolean?) boolean?)]
  [Grid-any
   (-> (Grid/c any/c) (-> any/c boolean?) boolean?)]
  [Grid-set-at
   (->i ([grid (Grid/c any/c)]
         [position (grid) (Grid-position/c grid)]
         [new-value any/c])
        [result (position new-value)
         (and/c (Grid/c any/c)
                (λ (grid) (equal? (Grid-at grid position) new-value)))])]
  [Grid-merge
   (->i ([grid1 (Grid/c any/c)]
         [grid2 (Grid/c any/c)]
         [choose (-> any/c any/c boolean?)])
        [result (grid1 grid2) (Grid/c (or/c (Grid-member/c grid1)
                                            (Grid-member/c grid2)))])]
  [Grid-indices
   (->i ([grid (Grid/c any/c)])
        [result (grid) (Grid/c (Grid-position/c grid))])]
  [Grid-enumerate
   (->i ([grid (Grid/c any/c)])
        [result (grid) (and/c (Grid/c (cons/c (Grid-position/c grid) (Grid-member/c grid)))
                              (property/c (curry Grid-map car) (equal/c (Grid-indices grid)))
                              (property/c (curry Grid-map cdr) (equal/c grid)))])]))
