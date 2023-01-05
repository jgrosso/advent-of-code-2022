#lang racket

(define/contract (total xs)
  ((listof number?) . -> . number?)
  (apply + xs))

(define/contract choice/c
  flat-contract?
  (symbols 'rock 'paper 'scissors))

(define/contract round/c
  flat-contract?
  (cons/c choice/c choice/c))

(define/contract opponent-choice
  (round/c . -> . choice/c)
  car)

(define/contract my-choice
  (round/c . -> . choice/c)
  cdr)

(define/contract winner/c
  flat-contract?
  (symbols 'opponent 'draw 'me))

(define/contract strategy-guide/c
  flat-contract?
  (listof round/c))

(define/contract (parse-opponent-choice text)
  ((or/c "A" "B" "C") . -> . choice/c)
  (match text
    ["A" 'rock]
    ["B" 'paper]
    ["C" 'scissors]))

(define/contract (parse-my-choice text)
  ((or/c "X" "Y" "Z") . -> . choice/c)
  (match text
    ["X" 'rock]
    ["Y" 'paper]
    ["Z" 'scissors]))

(define/contract (parse-round text)
  (string? . -> . round/c)
  (match (string-split text " ")
    [(list opponent-choice-text my-choice-text)
     (cons (parse-opponent-choice opponent-choice-text)
           (parse-my-choice my-choice-text))]))

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

(define/contract (beats? choice1 choice2)
  (-> choice/c choice/c boolean?)
  (equal? choice1 (beat choice2)))

(define/contract (winner round)
  (round/c . -> . winner/c)
  (cond
    [((opponent-choice round) . beats? . (my-choice round)) 'opponent]
    [((my-choice round) . beats? . (opponent-choice round)) 'me]
    [else                                                   'draw]))

(define/contract (score-round round)
  (round/c . -> . exact-positive-integer?)
  (let ([outcome-score
         (match (winner round)
           ['opponent 0]
           ['draw     3]
           ['me       6])])
    (outcome-score . + . (score-choice (my-choice round)))))

(define/contract (score-strategy-guide strategy-guide)
  (strategy-guide/c . -> . exact-nonnegative-integer?)
  (total (map score-round strategy-guide)))

(define/contract strategy-guide
  strategy-guide/c
  (parse-strategy-guide (port->string (open-input-file "input"))))

(score-strategy-guide strategy-guide)
