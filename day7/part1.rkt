#lang racket

(define (total xs)
  (apply + xs))

(define (lines text)
  (string-split text "\n"))

(define (read-string text)
  (read (open-input-string text)))

(define root-path (list))

(define filesystem (make-hash))
(define current-path root-path)

(define file? number?)
(define dir? hash?)

(define (at path)
  (define (go path node)
    (if (null? path)
        node
        (go (cdr path) (hash-ref node (car path)))))
  (go (reverse path) filesystem))

(define (add-child-dir! dir-name)
  (hash-set! (at current-path) dir-name (make-hash)))

(define (add-child-file! file-name file-size)
  (hash-set! (at current-path) file-name file-size))

(define (fold/sum node f)
  (+ (f node)
     (cond
       [(file? node) 0]
       [(dir? node)
        (total (map (λ (node) (fold/sum node f))
                    (hash-values node)))])))

(define (size node)
  (fold/sum node
            (λ (node)
              (cond
                [(file? node) node]
                [(dir? node) 0]))))

(for ([block (string-split (port->string (open-input-file "input"))
                           "$ ")])
  (match (lines block)
    [(cons command output-lines)
     (match (string-split command)
       [(list "cd" dir)
        (set! current-path (match dir
                             ["/" root-path]
                             [".." (cdr current-path)]
                             [dir (cons dir current-path)]))]
       [(list "ls")
        (for ([output-line output-lines])
          (match (string-split output-line)
            [(list "dir" child-dir-name)
             (add-child-dir! child-dir-name)]
            [(list file-size file-name)
             (add-child-file! file-name (read-string file-size))]))])]))

(fold/sum filesystem
          (λ (node)
            (cond
              [(file? node) 0]
              [(dir? node)
               (let ([dir-size (size node)])
                 (if (<= dir-size 100000)
                     dir-size
                     0))])))
