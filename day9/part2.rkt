#lang racket

(define (repeat x n)
  (if (zero? n)
      (list)
      (cons x (repeat x (sub1 n)))))

(define (scanl f initial xs)
  (reverse (foldl (λ (x acc)
                    (let ([new-value (f x (car acc))])
                      (cons new-value acc)))
                  (list initial)
                  xs)))

(define (read-from-string input-string)
  (read (open-input-string input-string)))

(define (point-x point)
  (car point))

(define (point-y point)
  (cdr point))

(define point-zero
  (cons 0 0))

(define (point-+ point1 point2)
  (cons (+ (point-x point1) (point-x point2))
        (+ (point-y point1) (point-y point2))))

(define (touching? point1 point2)
  (< (+ (expt (- (point-x point1) (point-x point2)) 2)
        (expt (- (point-y point1) (point-y point2)) 2))
     4))

(define (rope-head rope)
  (car rope))

(define (rope-tail rope)
  (last rope))

(define (propagate tail head)
  (let*
      ([delta
        (if (touching? head tail)
            point-zero
            (cons (sgn (- (point-x head) (point-x tail)))
                  (sgn (- (point-y head) (point-y tail)))))])
    (point-+ tail delta)))

(define (pull head direction)
  (let*
      ([delta
        (match direction
          ['right (cons 1 0)]
          ['left (cons -1 0)]
          ['up (cons 0 -1)]
          ['down (cons 0 1)])])
    (point-+ head delta)))

(define (parse-direction direction-string)
  (match direction-string
    ["R" 'right]
    ["L" 'left]
    ["U" 'up]
    ["D" 'down]))

(define (parse-instructions input-port)
  (append*
   (for/list
      ([line (in-lines input-port)])
     (match-let*
         ([(list direction num-steps) (string-split line)]
          [direction (parse-direction direction)]
          [num-steps (read-from-string num-steps)])
       (repeat direction num-steps)))))

(define (execute-instruction direction rope)
  (scanl propagate
         (pull (rope-head rope) direction)
         (cdr rope)))

(let* ([rope-history
        (scanl execute-instruction
               (repeat point-zero 10)
               (parse-instructions (open-input-file "input")))]
       [tail-history (map rope-tail rope-history)]
       [tail-visited (list->set tail-history)])
  (set-count tail-visited))
