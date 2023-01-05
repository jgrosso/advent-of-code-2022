#lang racket

(define/contract (total xs)
  ((listof number?) . -> . number?)
  (apply + xs))

(define/contract choice/c
  flat-contract?
  (symbols 'rock 'paper 'scissors))

(define/contract winner/c
  flat-contract?
  (symbols 'opponent 'draw 'me))

(define/contract round/c
  flat-contract?
  (cons/c choice/c winner/c))

(define/contract opponent-choice
  (round/c . -> . choice/c)
  car)

(define/contract winner
  (round/c . -> . winner/c)
  cdr)

(define/contract strategy-guide/c
  flat-contract?
  (listof round/c))

(define/contract (parse-opponent-choice text)
  ((or/c "A" "B" "C") . -> . choice/c)
  (match text
    ["A" 'rock]
    ["B" 'paper]
    ["C" 'scissors]))

(define/contract (parse-winner text)
  ((or/c "X" "Y" "Z") . -> . winner/c)
  (match text
    ["X" 'opponent]
    ["Y" 'draw]
    ["Z" 'me]))

(define/contract (parse-round text)
  (string? . -> . round/c)
  (match (string-split text " ")
    [(list opponent-choice-text winner-text)
     (cons (parse-opponent-choice opponent-choice-text)
           (parse-winner winner-text))]))

(define/contract (parse-strategy-guide text)
  (string? . -> . strategy-guide/c)
  (map parse-round (string-split text "\n")))

(define/contract (score-choice choice)
  (choice/c . -> . exact-positive-integer?)
  (match choice
    ['rock     1]
    ['paper    2]
    ['scissors 3]))

(define/contract (beat choice)
  (-> choice/c choice/c)
  (match choice
    ['rock     'paper]
    ['paper    'scissors]
    ['scissors 'rock]))

(define/contract (lose-to choice)
  (-> choice/c choice/c)
  (match choice
    ['paper    'rock]
    ['scissors 'paper]
    ['rock     'scissors]))

(define/contract (score-round round)
  (round/c . -> . exact-positive-integer?)
  (let ([outcome-score
         (match (winner round)
           ['opponent 0]
           ['draw     3]
           ['me       6])]
        [my-choice
         (match (winner round)
           ['opponent (lose-to (opponent-choice round))]
           ['draw     (opponent-choice round)]
           ['me       (beat (opponent-choice round))])])
    (outcome-score . + . (score-choice my-choice))))

(define/contract (score-strategy-guide strategy-guide)
  (strategy-guide/c . -> . exact-nonnegative-integer?)
  (total (map score-round strategy-guide)))

(define/contract strategy-guide
  strategy-guide/c
  (parse-strategy-guide (port->string (open-input-file "input"))))

(score-strategy-guide strategy-guide)
