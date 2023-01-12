#lang racket

(require "contract.rkt")

(define (flip f)
  (λ (y x) (f x y)))

(define (member-of/c xs)
  (λ (x) (member x xs)))

(define (total ns)
  (foldl + 0 ns))

(define (product ns)
  (foldl * 1 ns))

(define (repeat x n)
  (if (zero? n)
      (list)
      (cons x (repeat x (sub1 n)))))

(define (scanl f initial xs)
  (foldl (λ (x acc)
           (let ([new-value (f x (car acc))])
             (cons new-value acc)))
         (list initial)
         xs))

(define (chunk xs n)
  (if (null? xs)
      null
      (cons (take xs n)
            (chunk (drop xs n) n))))

(define (pairs xs)
  (if (< (length xs) 2)
      null
      (cons (take xs 2)
            (pairs (cdr xs)))))

(define (lines text)
  (string-split text "\n" #:repeat? #t))

(define (indices xs)
  (range 0 (length xs)))

(define (enumerate xs)
  (map (λ (i) (cons i (list-ref xs i)))
       (indices xs)))

(define (sorted? xs . args)
  (equal? xs (apply sort xs args)))

(define (sorted/c . args)
  (λ (xs) (apply sorted? xs args)))

(define (equal/c x)
  (λ (y) (equal? x y)))

(define (divisible/c m)
  (and/c exact-integer?
         (λ (n) (zero? (modulo n m)))))

(define (fixed-point-from f init)
  (let ([new (f init)])
    (if (equal? init new)
        new
        (fixed-point-from f new))))

(provide/c
 (contract-out
  [member-of/c
   (-> list? contract?)]
  [flip
   (-> (-> any/c any/c any/c) (-> any/c any/c any/c))]
  [total
   (-> (listof number?) number?)]
  [product
   (-> (listof number?) number?)]
  [repeat
   (->i ([x any/c]
         [n exact-nonnegative-integer?])
        [result (x n)
                (and/c (listof (equal/c x))
                       (property/c length (=/c n)))])]
  [scanl
   (-> (-> any/c any/c any/c) any/c list? list?)]
  [chunk
   (->i ([xs (n)
             (and/c list?
                    (property/c length (divisible/c n)))]
         [n exact-nonnegative-integer?])
        [result (xs n)
                (and/c (listof (and/c list?
                                      (property/c length (=/c n))))
                       (property/c flatten (equal/c xs)))])]
  [pairs
   (->i ([xs list?])
        [result (xs)
                (and/c (listof (and/c (listof (member-of/c xs))
                                      (property/c length (=/c 2))))
                       (property/c length (=/c (sub1 (length xs)))))])]
  [divisible/c (-> exact-nonnegative-integer? contract?)]
  [lines
   (-> string?
       (listof (and/c string?
                      (not/c (λ (s) (string-contains? s "\n"))))))]
  [sorted?
   (->* (list?) () #:rest any/c boolean?)]
  [sorted/c
   (->* () () #:rest any/c contract?)]
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
                       (property/c (curry map cdr) (equal/c xs)))])]
  [fixed-point-from
   (-> (-> any/c any/c) any/c any/c)]))
