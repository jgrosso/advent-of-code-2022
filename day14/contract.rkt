#lang racket

(require (for-syntax racket)
         (for-syntax syntax/parse))

(define-for-syntax enable-contracts? #f)

(define-for-syntax (strip-contract-from stx)
  (syntax-parse stx
    #:literals (rename struct)
    [#:unprotected-submodule #'()]
    [(struct struct-id:id _ ...)
     #'(struct-out struct-id)]
    [(rename orig-id:id id:id _ ...)
     #'(rename-out [orig-id id])]
    [(name:id _ ...)
     #'name]
    [#:∃ #'()]
    [#:exists #'()]
    [#:∀ #'()]
    [#:forall #'()]))

(define-syntax (provide/c stx)
  (syntax-parse stx
    [(_ provide-form:expr ...)
     (if enable-contracts?
         #'(provide provide-form ...)
         (with-syntax
           ([(new-provide-form ...)
             (append-map (λ (stx)
                           (syntax-parse stx
                             #:literals (contract-out)
                             [(contract-out contract-out-form:expr ...)
                              (map strip-contract-from
                                   (syntax->list #'(contract-out-form ...)))]
                             [_ (list stx)]))
                         (syntax->list #'(provide-form ...)))])
           #'(provide new-provide-form ...)))]))

(provide/c
 provide/c)
