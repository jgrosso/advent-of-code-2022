#lang racket

(define (read-from-char char)
  (read (open-input-string (string char))))

(define (row grid row-index)
  (list-ref grid row-index))

(define (column grid column-index)
  (for/list ([i (range 0 (length grid))])
    (let ([row (list-ref grid i)])
      (list-ref row column-index))))

(define (rows grid) grid)

(define (columns grid)
  (for/list ([column-index (range 0 (length grid))])
    (column grid column-index)))

(define tree-position car)
(define tree-height cdr)

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

(let* ([grid (parse-grid (port->string (open-input-file "input")))]
       [visible-trees (apply set-union
                             (map trees-visible-from-sides-of
                                  (append (rows grid) (columns grid))))])
  (set-count visible-trees))
