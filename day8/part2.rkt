#lang racket

(define (read-from-char char)
  (read (open-input-string (string char))))

(define (row grid row-index)
  (list-ref grid row-index))

(define (column grid column-index)
  (for/list ([i (range 0 (length grid))])
    (let ([row (list-ref grid i)])
      (list-ref row column-index))))

(define (rows grid)
  grid)

(define (columns grid)
  (for/list ([column-index (range 0 (length grid))])
    (column grid column-index)))

(define (row-index position)
  (car position))

(define (column-index position)
  (cdr position))

(define (at grid position)
  (list-ref (row grid (row-index position))
            (column-index position)))

(define (tree-position tree)
  (car tree))

(define (tree-height tree)
  (cdr tree))

(define (trees-visible-from-beginning-of trees)
  (for/fold ([max-tree-height -1]
             [visible-trees (set)]
             #:result visible-trees)
            ([tree trees])
    (if (< max-tree-height (tree-height tree))
        (values (tree-height tree)
                (set-add visible-trees (tree-position tree)))
        (values max-tree-height visible-trees))))

(define (trees-visible-from-sides-of trees)
  (set-union (trees-visible-from-beginning-of trees)
             (trees-visible-from-beginning-of (reverse trees))))

(define (parse-row text row-index)
  (let ([heights (string->list text)])
    (for/list ([column-index (range 0 (length heights))])
      (let ([position (cons row-index column-index)]
            [height (read-from-char (list-ref heights column-index))])
        (cons position height)))))

(define (parse-grid text)
  (let ([lines (string-split text "\n")])
    (for/list ([row-index (range 0 (length lines))])
      (let ([line (list-ref lines row-index)])
        (parse-row line row-index)))))

(define (scenic-score-from-beginning-of grid tree trees)
  (match trees
    [(list) 0]
    [(cons other-tree trees)
     (cond
       [(< (tree-height other-tree) (tree-height tree))
        (add1 (scenic-score-from-beginning-of grid tree trees))]
       [else 1])]))

(define (scenic-score grid tree)
  (let* ([row (row-index (tree-position tree))]
         [col (column-index (tree-position tree))]
         [trees-to-left
          (for/list ([i (range (sub1 col) -1 -1)])
            (at grid (cons row i)))]
         [trees-to-right
          (for/list ([i (range (add1 col) (length grid))])
            (at grid (cons row i)))]
         [trees-to-top
          (for/list ([i (range (sub1 row) -1 -1)])
            (at grid (cons i col)))]
         [trees-to-bottom
          (for/list ([i (range (add1 row) (length grid))])
            (at grid (cons i col)))])
    (* (scenic-score-from-beginning-of grid tree trees-to-left)
       (scenic-score-from-beginning-of grid tree trees-to-right)
       (scenic-score-from-beginning-of grid tree trees-to-top)
       (scenic-score-from-beginning-of grid tree trees-to-bottom))))

(define (grid->list grid)
  (apply append (rows grid)))

(let* ([grid (parse-grid (port->string (open-input-file "input")))])
  (apply max (map (λ (tree) (scenic-score grid tree)) (grid->list grid))))
