#lang racket

(provide (all-defined-out))

(define (set-filter pred set)
  (list->set (filter pred (set->list set))))

(define (filter-item-set item-data item-set pred)
  (set-filter (λ (item)
                (pred item-data item))
              item-set))

(define-syntax (thread stx)
  (syntax-case stx ()
    ((thread x) #'x)
    ((thread x (f ...) tail ...)
     #'(thread (f ... x) tail ...))))

(define-syntax (thread-first stx)
  (syntax-case stx ()
    ((thread-first x) #' x)
    ((thread-first x (f args ...) tail ...)
     #'(thread-first (f x args ...) tail ...))))

(define (hash-filter-keys hash pred)
  (make-hash
   (map (λ (k) (cons k (hash-ref hash k)))
        (filter identity
                (hash-map hash
                          (λ (k v)
                            (if (pred k) k #f)))))))