#lang racket

(provide (all-defined-out))

(define (set-filter pred set)
  (list->set (filter pred (set->list set))))

(define (filter-item-set item-data item-set pred)
  (set-filter (λ (item)
                (pred item-data item))
              item-set))
