#lang racket

(require "item.rkt")

(provide (all-defined-out))

(define (urgency item)
  (item-base-urgency item))

(define (sort-items-by-urgency-descending items)
  (sort (set->list items)
        (λ (a b)
          (>= (urgency a)
              (urgency b)))))