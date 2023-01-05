#lang racket

(define-syntax-rule (match-define/contract name c clause ...)
  (define/contract name
    c
    (match-lambda
      clause ...)))

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

(define/contract compartment/c
  flat-contract?
  (listof type/c))

(struct rucksack (left right)
  #:transparent)

(define/contract rucksack/c
  flat-contract?
  (match-lambda
    [(rucksack left right)
     (and
      (compartment/c left)
      (compartment/c right)
      (equal? (length left) (length right)))]
    [_ #f]))

(define/contract rucksack-source/c
  flat-contract?
  (λ (text)
    (and
     (string? text)
     (even? (string-length text))
     (andmap type/c (string->list text)))))

(define/contract (parse-rucksack text)
  (rucksack-source/c . -> . rucksack/c)
  (let* ([midpoint ((string-length text) . / . 2)]
         [left (substring text 0 midpoint)]
         [right (substring text midpoint)])
    (rucksack (string->list left) (string->list right))))

(define/contract (parse-rucksacks text)
  (string? . -> . (listof rucksack/c))
  (map parse-rucksack (string-split text "\n")))

(match-define/contract find-common
  (rucksack/c . -> . type/c)
  [(rucksack left right)
   (first (intersection left right))])

(define/contract (priority type)
  (type/c . -> . exact-positive-integer?)
  (+ (char->integer type)
     (cond
       [(char-lower-case? type) (1  . - . (char->integer #\a))]
       [(char-upper-case? type) (27 . - . (char->integer #\A))])))

(define/contract rucksacks
  (listof rucksack/c)
  (parse-rucksacks (port->string (open-input-file "input"))))

(total (map (compose1 priority find-common) rucksacks))
