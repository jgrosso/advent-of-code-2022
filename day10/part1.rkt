#lang racket

(define (scanl f initial xs)
  (reverse (foldl (λ (x acc)
                    (let ([new-value (f x (car acc))])
                      (cons new-value acc)))
                  (list initial)
                  xs)))

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

(define (signal-strength-during history cycle-num)
  (* cycle-num (list-ref history (sub1 cycle-num))))

(let* ([instructions (parse-instructions (open-input-file "input"))]
       [history (execute-instructions instructions)])
  (for/sum ([cycle-num (list 20 60 100 140 180 220)])
    (signal-strength-during history cycle-num)))
