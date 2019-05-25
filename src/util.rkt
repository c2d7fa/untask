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

;; Returns (values removed added), where removed is a set containing
;; all values in old-set but not new-set and added is a set containing
;; all values in new-set but not old-set.
(define (set-diff old-set new-set)
  (define removed
    (set-filter (λ (x)
                  (and (set-member? old-set x) (not (set-member? new-set x))))
                old-set))
  (define added
    (set-filter (λ (x)
                  (and (set-member? new-set x) (not (set-member? old-set x))))
                new-set))
  (values removed added))
