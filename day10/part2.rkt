#lang racket

(define (scanl f initial xs)
  (reverse (foldl (λ (x acc)
                    (let ([new-value (f x (car acc))])
                      (cons new-value acc)))
                  (list initial)
                  xs)))

(define (chunk xs n)
  (if (null? xs)
      null
      (cons (take xs n)
            (chunk (drop xs n) n))))

(define (list-init xs)
  (take xs (- (length xs) 1)))

(define (read-from-string input-string)
  (read (open-input-string input-string)))

(define (parse-instructions input-port)
  (for/list ([line (in-lines input-port)])
    (match (string-split line)
      [(list "noop") (list 'noop)]
      [(list "addx" X-delta) (list 'addx (read-from-string X-delta))])))

(define (execute-instructions instructions)
  (reverse
   (foldl (λ (instr history)
            (let* ([X (car history)]
                   [new-steps
                    (match instr
                      [(list 'noop)
                       (list X)]
                      [(list 'addx X-delta)
                       (list (+ X X-delta) X)])])
              (append new-steps history)))
          (list 1)
          instructions)))

(define (draw-sprite-at? column-index X)
  (<= (sub1 X) column-index (add1 X)))

(define (draw-row history-row)
  (list->string
   (for/list ([column-index (range 0 (length history-row))])
     (let ([X (list-ref history-row column-index)])
       (if (draw-sprite-at? column-index X)
           #\#
           #\.)))))

(let* ([row-length 40]
       [instructions (parse-instructions (open-input-file "input"))]
       [history (list-init (execute-instructions instructions))]
       [history-rows (chunk history row-length)])
  (display (string-join (map draw-row history-rows) "\n")))
