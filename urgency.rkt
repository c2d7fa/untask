#lang racket

(provide (all-defined-out))

(require (prefix-in data: "item-data.rkt"))

(define base-urgency-key 'base-urgency)

(define (register-property-base-urgency item-data)
  (data:new-property item-data #:key base-urgency-key #:name "Base Urgency" #:default 0))

(define (urgency item-data item)
  (data:get-property item-data item base-urgency-key))

;; Takes item-data and a set of items, returns sorted list of items.
(define (sort-items-by-urgency-descending item-data item-set)
  (sort (set->list item-set) >=
        #:key (λ (item)
                (urgency item-data item))))
