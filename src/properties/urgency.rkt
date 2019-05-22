#lang racket

(provide (all-defined-out))

(require
  (prefix-in item: "../data/item-data.rkt")
  (prefix-in prop: "../data/property-type.rkt")
  (prefix-in val: "../data/values.rkt"))

(define base-urgency-property-type
  (prop:make-property-type #:key 'baseurgency
                           #:default (val:make-number 0)))

(define (calculate-urgency item-data item)
  (item:get-property item-data item base-urgency-property-type))

;; Takes item-data and a set of items, returns sorted list of items.
(define (sort-items-by-urgency-descending item-data item-set)
  (define (number>= x y)
    (>= (val:unwrap-number x) (val:unwrap-number y)))
  (sort (set->list item-set) number>=
        #:key (λ (item)
                (calculate-urgency item-data item))))