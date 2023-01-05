#lang racket

(require racket/format)

(define (read-string text)
  (read (open-input-string text)))

(define tower-width 4)

(define (vector-modify! vector index f)
  (vector-set! vector index (f (vector-ref vector index))))

(define (push-crate! towers index crate)
  (vector-modify! towers index (λ (tower) (cons crate tower))))

(define (pop-crate! towers index)
  (let ([crate (car (vector-ref towers index))])
    (vector-modify! towers index cdr)
    crate))

(define (parse-towers text)
  (match-let*-values
      ([(lines)
        (string-split text "\n")]
       [(towers-source-lines (list legend))
        (split-at lines (- (length lines) 1))]
       [(num-towers)
        (read-string (last (string-split legend)))]
       [(max-line-length)
        (* num-towers tower-width)]
       [(towers)
        (make-vector num-towers (list))])
    (for* ([line-index (range 0 (length towers-source-lines))]
           [tower-index (range 0 (/ max-line-length tower-width))])
      (let* ([normalized-line
              (~a
               (list-ref towers-source-lines
                         (- (length towers-source-lines) 1 line-index))
               #:width max-line-length)]
             [row
              (substring normalized-line
                         (* tower-index       tower-width)
                         (* (+ tower-index 1) tower-width))])
        (match (string-ref row 0)
          [#\[
           (let ([crate (string-ref row 1)])
             (push-crate! towers tower-index crate))]
          [else (void)])))
    towers))

(define (move-crate! towers origin destination)
  (let ([crate (pop-crate! towers origin)])
    (push-crate! towers destination crate)))

(define (move-crates! towers origin destination num-crates)
  (unless (zero? num-crates)
    (move-crate! towers origin destination)
    (move-crates! towers origin destination (- num-crates 1))))

(define (execute-instruction! towers instruction)
  (match instruction
    [(list num-crates origin destination)
     (move-crates! towers origin destination num-crates)]))

(define (parse-instruction text)
  (match (string-split text)
    [(list _ num-crates _ origin _ destination)
     (list (read-string num-crates)
           (- (read-string origin) 1)
           (- (read-string destination) 1))]))

(match (string-split (port->string (open-input-file "input")) "\n\n")
  [(list towers-source instructions-source)
   (let ([towers (parse-towers towers-source)])
     (for ([instruction-source (string-split instructions-source "\n")])
       (execute-instruction! towers (parse-instruction instruction-source)))
     (string-join (vector->list
                   (vector-map (λ (tower) (string (car tower)))
                               towers))
                  ""))])
