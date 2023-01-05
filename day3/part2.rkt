#lang racket

(define-syntax-rule (match-define/contract name c clause ...)
  (define/contract name
    c
    (match-lambda
      clause ...)))

(define/contract (all/c c)
  (contract? . -> . flat-contract?)
  (λ (xs) (andmap (flat-contract-predicate c) xs)))

(define/contract (divisible/c m)
  (exact-integer? . -> . flat-contract?)
  (and/c exact-integer?
         (λ (n) (zero? (modulo n m)))))

(define/contract (chunk n xs)
  (->i ([n exact-nonnegative-integer?]
        [xs (n) (and/c (listof any/c)
                       (property/c length (divisible/c n)))])
       [result (n xs) (and/c
                       (listof (listof any/c))
                       (all/c (property/c length (=/c n))))])
  (if (null? xs)
      null
      (cons (take xs n) (chunk n (drop xs n)))))

(define/contract (total xs)
  ((listof number?) . -> . number?)
  (apply + xs))

(define/contract (intersection xs ys)
  (-> (listof any/c) (listof any/c) (listof any/c))
  (filter (λ (x) (member x ys)) xs))

(define/contract (type/c char)
  flat-contract?
  (and/c
   char?
   (λ (char) (char-ci<=? #\a char #\z))))

(define/contract rucksack/c
  flat-contract?
  (listof type/c))

(define/contract rucksack-source/c
  flat-contract?
  (and/c
    string?
    (property/c string->list (all/c type/c))))

(define/contract group-size
  exact-positive-integer?
  3)

(define/contract group/c
  flat-contract?
  (list/c rucksack/c rucksack/c rucksack/c))

(define/contract (parse-group rucksack-sources)
  (->
   (and/c (listof type/c)
          (property/c length (=/c group-size)))
   group/c)
  (map string->list rucksack-sources))

(define/contract (parse-groups text)
  (string? . -> . (listof group/c))
  (map parse-group (chunk group-size (string-split text "\n"))))

(match-define/contract find-common
  (group/c . -> . type/c)
  [(list a b c)
   (first (intersection (intersection a b) c))])

(define/contract (priority type)
  (type/c . -> . exact-positive-integer?)
  (+ (char->integer type)
     (cond
       [(char-lower-case? type) (1  . - . (char->integer #\a))]
       [(char-upper-case? type) (27 . - . (char->integer #\A))])))

(define/contract rucksacks
  (listof group/c)
  (parse-groups (port->string (open-input-file "input"))))

(total (map (compose1 priority find-common) rucksacks))
