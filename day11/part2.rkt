#lang racket

(define (divides? m n)
  (= (modulo m n) 0))

(define (fix f n x)
  (if (= n 1)
      (f x)
      (f (fix f (sub1 n) x))))

(define divisor-product 0)

(struct Inspect-Algorithm (operation
                           operand))

(define (Inspect-Algorithm-run inspect-algorithm item)
  ((Inspect-Algorithm-operation inspect-algorithm)
   item
   (match (Inspect-Algorithm-operand inspect-algorithm)
     ['old item]
     [n n])))

(struct Throw-Algorithm (divisor
                         recipient-if-true
                         recipient-if-false))

(define (Throw-Algorithm-run throw-algorithm item)
  (if (divides? item
                (Throw-Algorithm-divisor throw-algorithm))
      (Throw-Algorithm-recipient-if-true throw-algorithm)
      (Throw-Algorithm-recipient-if-false throw-algorithm)))

(struct Monkey (inspect-algorithm
                throw-algorithm
                items
                num-inspects))

(define (Monkey-inspect monkey item)
  (modulo (Inspect-Algorithm-run (Monkey-inspect-algorithm monkey)
                                 item)
          divisor-product))

(define (Monkey-throw monkey item)
  (Throw-Algorithm-run (Monkey-throw-algorithm monkey) item))

(define (Monkey-catch monkey item)
  (struct-copy Monkey monkey
               [items (append (Monkey-items monkey) (list item))]))

(define (Monkey-turn monkey-num monkeys)
  (let* ([monkey (list-ref monkeys monkey-num)]
         [items (Monkey-items monkey)])
    (match items
      [(list) monkeys]
      [(cons item new-items)
       (let*
           ([new-monkey (struct-copy Monkey monkey
                                     [items new-items]
                                     [num-inspects (add1 (Monkey-num-inspects monkey))])]
            [inspected-item (Monkey-inspect monkey item)]
            [recipient-num (Monkey-throw monkey inspected-item)]
            [recipient (list-ref monkeys recipient-num)]
            [new-recipient (Monkey-catch recipient inspected-item)]
            [new-monkeys
             (list-set (list-set monkeys recipient-num new-recipient)
                       monkey-num new-monkey)])
         (Monkey-turn monkey-num new-monkeys))])))

(define (display-monkeys monkeys)
  (for ([i (range 0 (length monkeys))])
    (display "Monkey ") (display i) (display ": ")
    (for ([item (Monkey-items (list-ref monkeys i))])
      (display item) (display " "))
    (display "\n")))

(define (round monkeys)
  (foldl Monkey-turn
         monkeys
         (range 0 (length monkeys))))

(define (monkey-business monkeys)
  (apply * (take (sort (map Monkey-num-inspects monkeys)
                       >)
                 2)))

(define (parse-starting-items text)
  (match (string-split text)
    [(list "Starting" "items:" items ...)
     (map (λ (item) (string->number (string-trim item ","))) items)]))

(define (parse-operator text)
  (match text
    ["+" +]
    ["*" *]))

(define (parse-operand text)
  (if (equal? text "old")
      'old
      (string->number text)))

(define (parse-Inspect-Algorithm text)
  (match (string-split text)
    [(list "Operation:" "new" "=" "old" operator operand)
     (Inspect-Algorithm (parse-operator operator) (parse-operand operand))]))

(define (parse-Throw-Algorithm lines)
  (match (map string-split lines)
    [(list (list "Test:" "divisible" "by" divisor)
           (list "If" "true:" "throw" "to" "monkey" recipient-if-true)
           (list "If" "false:" "throw" "to" "monkey" recipient-if-false))
     (Throw-Algorithm (string->number divisor)
                      (string->number recipient-if-true)
                      (string->number recipient-if-false))]))

(define (parse-Monkey text)
  (match (string-split text "\n")
    [(list _
           starting-items
           inspect-algorithm
           throw-algorithm-lines ...)
     (Monkey (parse-Inspect-Algorithm inspect-algorithm)
             (parse-Throw-Algorithm throw-algorithm-lines)
             (parse-starting-items starting-items)
             0)]))

(define (parse-monkeys text)
  (map parse-Monkey (string-split text "\n\n")))

(let* ([num-rounds 10000]
       [initial-monkeys (parse-monkeys (port->string (open-input-file "input")))])
  (set! divisor-product
        (apply * (map (λ (monkey) (Throw-Algorithm-divisor (Monkey-throw-algorithm monkey)))
                      initial-monkeys)))
  (let ([final-monkeys (fix round num-rounds initial-monkeys)])
    (monkey-business final-monkeys)))
