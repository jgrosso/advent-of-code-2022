#lang racket

(define (repeat x n)
  (if (zero? n)
      (list)
      (cons x (repeat x (sub1 n)))))

(define (lines text)
  (string-split text "\n"))

(define (indices xs)
  (range 0 (length xs)))

(define (enumerate xs)
  (map (λ (i) (cons i (list-ref xs i)))
       (indices xs)))

(define (sorted/c comp [extract-key (λ (x) x)])
  (λ (xs) (=/c xs (sort xs comp extract-key))))

(define (equal/c x)
  (λ (y) (equal? x y)))

(provide
 (contract-out
  [repeat
   (->i ([x any/c]
         [n exact-nonnegative-integer?])
        [result (x n)
                (and/c (listof (equal/c x))
                       (property/c length (=/c n)))])]
  [lines
   (-> string? (listof (and/c string?
                              (not/c (λ (s) (string-contains? s "\n"))))))]
  [sorted/c
   (->i ([comp (-> any/c any/c boolean?)])
        ([extract-key (-> any/c any/c)])
        [result contract?])]
  [equal/c
   (-> any/c contract?)]
  [indices
   (->i ([xs (listof any/c)])
        [result (xs)
                (and/c (listof exact-nonnegative-integer?)
                       (sorted/c <)
                       (property/c check-duplicates #f)
                       (property/c length (=/c (length xs)))
                       (property/c min (=/c 0))
                       (property/c max (=/c (sub1 (length xs)))))])]
  [enumerate
   (->i ([xs (listof any/c)])
        [result (xs)
                (and/c (listof (cons/c exact-nonnegative-integer? any/c))
                       (property/c (curry map car) (equal/c (indices xs)))
                       (property/c (curry map cdr) (equal/c xs)))])]))
