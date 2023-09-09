#lang racket

(require "contract.rkt"
         "position.rkt"
         "utility.rkt")

(struct Grid (matrix))

(define (matrix->Grid matrix)
  (Grid matrix))

(define (make-Grid width height init)
  (matrix->Grid (repeat (repeat init width)
                        height)))

(define (Grid-height grid)
  (length (Grid-matrix grid)))

(define (Grid-width grid)
  (let ([rows (Grid-matrix grid)])
    (if (null? rows)
        0
        (length (car rows)))))

(define (Grid-area grid)
  (* (Grid-width grid) (Grid-height grid)))

(define (Grid->string grid [element->string ~a])
  (let* ([naive-rendering
          (Grid-map element->string grid)]
         [max-column-width
          (Grid-fold (Grid-map string-length naive-rendering)
                     max 0)]
         [aligned-rendering
          (map (λ (s) (~a s #:width max-column-width))
               (Grid-matrix naive-rendering))])
    (string-join (map (λ (x) (string-join x "")) aligned-rendering)
                 "\n")))

(define (Grid-inside? grid position)
  (and (<= 0 (Position-x position) (sub1 (Grid-width grid)))
       (<= 0 (Position-y position) (sub1 (Grid-height grid)))))

(define (Grid-at grid position)
  (let ([row (list-ref (Grid-matrix grid) (Position-y position))])
    (list-ref row (Position-x position))))

(define (Grid-map proc . grids)
  (matrix->Grid (apply (curry map (curry map proc))
                       (map Grid-matrix grids))))

(define (Grid-fold grid proc init)
  (foldl (λ (row acc) (foldl proc acc row))
         init
         (Grid-matrix grid)))

(define (Grid-indices grid)
  (matrix->Grid (for/list ([row-index (range 0 (Grid-height grid))])
                  (for/list ([column-index (range 0 (Grid-width grid))])
                    (Position column-index row-index)))))

(define (Grid-enumerate grid)
  (Grid-map cons (Grid-indices grid) grid))

(define (Grid-and grid)
  (Grid-fold grid (λ (x acc) (and x acc)) true))

(define (Grid-or grid)
  (Grid-fold grid (λ (x acc) (or x acc)) false))

(define (Grid-sum grid)
  (Grid-fold grid + 0))

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
  (struct/c Grid (matrix-of/c elem/c)))

(define (Grid-member/c grid)
  (λ (x) (Grid-member? grid x)))

(define (inside-Grid/c grid)
  (λ (position) (Grid-inside? grid position)))

(define (matrix? rows)
  (or (null? rows)
      (let ([dimension (length (car rows))])
        (andmap (λ (row) (= (length row) dimension))
                rows))))

(define (matrix-of/c elem/c)
  (and/c (listof (listof elem/c))
         matrix?))

(define (Grid-Position/c grid)
  (and/c Position?
         (inside-Grid/c grid)))

(define (Grid-set-at grid position new-value)
  (matrix->Grid (list-update (Grid-matrix grid)
                             (Position-y position)
                             (λ (row) (list-set row (Position-x position) new-value)))))

(provide/c
 (contract-out
  [matrix?
   (-> (listof (listof any/c)) boolean?)]
  [matrix-of/c
   (-> contract? contract?)]
  [Grid/c
   (-> flat-contract? contract?)]
  [matrix->Grid
   (->i ([matrix matrix?])
        [result (matrix) (Grid/c (member-of/c (flatten matrix)))])]
  [Grid-member/c
   (-> (Grid/c any/c) contract?)]
  [Grid-Position/c
   (-> (Grid/c any/c) contract?)]
  [Grid->string
   (->i ([grid (Grid/c any/c)])
        ([element->string (grid)
                          (-> (Grid-member/c grid) string?)])
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
  [Grid-area
   (-> (Grid/c any/c) exact-nonnegative-integer?)]
  [Grid-inside?
   (-> (Grid/c any/c) Position? boolean?)]
  [Grid-at
   (->i ([grid (Grid/c any/c)]
         [position (grid) (Grid-Position/c grid)])
        [result (grid) (Grid-member/c grid)])]
  [Grid-member?
   (-> (Grid/c any/c) any/c boolean?)]
  [Grid-map
   (parametric->/c
    [A B]
    (->* ((-> A ... B))
         #:rest (listof (Grid/c A))
         (Grid/c B)))]
  [Grid-fold
   (parametric->/c
    [A B]
    (->i ([grid (Grid/c B)]
          [proc (grid) (-> (Grid-member/c grid) A A)]
          [init B])
         [result A]))]
  [Grid-and
   (-> (Grid/c boolean?) boolean?)]
  [Grid-or
   (-> (Grid/c boolean?) boolean?)]
  [Grid-sum
   (-> (Grid/c number?) number?)]
  [Grid-all
   (->i ([grid (Grid/c any/c)]
         [proc (grid) (-> (Grid-member/c grid) boolean?)])
        [result boolean?])]
  [Grid-any
   (->i ([grid (Grid/c any/c)]
         [proc (grid) (-> (Grid-member/c grid) boolean?)])
        [result boolean?])]
  [Grid-set-at
   (parametric->/c
    [A B]
    (->i ([grid (Grid/c A)]
                          [position (grid) (Grid-Position/c grid)]
                          [new-value B])
         [result (position new-value)
                 (and/c (Grid/c (or/c A B))
                        (λ (grid) (equal? (Grid-at grid position) new-value)))]))]
  [Grid-merge
   (parametric->/c
    [A B]
    (->i ([grid1 (Grid/c A)]
          [grid2 (Grid/c B)]
          [choose (-> A B boolean?)])
         [result (grid1 grid2)
                 (Grid/c (or/c (Grid-member/c grid1)
                               (Grid-member/c grid2)))]))]
  [Grid-indices
   (->i ([grid (Grid/c any/c)])
        [result (grid) (Grid/c (Grid-Position/c grid))])]
  [Grid-enumerate
   (->i ([grid (Grid/c any/c)])
        [result (grid)
                (and/c (Grid/c (cons/c (Grid-Position/c grid) (Grid-member/c grid)))
                       (property/c (curry Grid-map car) (equal/c (Grid-indices grid)))
                       (property/c (curry Grid-map cdr) (equal/c grid)))])]))
