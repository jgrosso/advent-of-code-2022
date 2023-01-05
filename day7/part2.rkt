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

(define (node-fold <> identity node proc)
  (let ([f (λ (node) (let ([value (proc node)])
                       (if (void? value)
                           identity
                           value)))])
    (<> (f node)
        (cond
          [(file? node) identity]
          [(dir? node)
           (apply <> identity
                  (map (λ (node)
                         (node-fold <> identity node f))
                       (hash-values node)))]))))

(define (size node)
  (node-fold + 0
             node
             (λ (node) (when (file? node) node))))

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

(let* ([disk-size 70000000]
       [total-free-space-required 30000000]
       [free-space-available (- disk-size (size filesystem))]
       [free-space-still-required (- total-free-space-required free-space-available)])
  (inexact->exact (node-fold min +inf.0
                             filesystem
                             (λ (node)
                               (let ([node-size (size node)])
                                 (when (and (dir? node)
                                            (>= node-size free-space-still-required))
                                   node-size))))))
