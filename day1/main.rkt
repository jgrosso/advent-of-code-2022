#lang racket

(define/contract (total xs)
  (-> (listof number?) number?)
  (apply + xs))

(define/contract elf?
  flat-contract?
  (listof exact-positive-integer?))

(define/contract (parse-elf text)
  (-> string? elf?)
  (for/list ([item (string-split text)])
    (read (open-input-string item))))

(define/contract (parse-elves text)
  (-> string? (listof elf?))
  (map parse-elf (string-split text "\n\n")))

(define/contract elves
  (listof elf?)
  (parse-elves (port->string (open-input-file "input"))))

(display "Part 1: ")
(display (apply max (map total elves)))
(display "\n")

(display "Part 2: ")
(display (apply + (take (sort (map total elves) >) 3)))
