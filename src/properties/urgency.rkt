#lang racket

(provide (all-defined-out))

(require (prefix-in data: "../data/item-data.rkt"))

(define base-urgency-key 'base-urgency)

(define (register-property-base-urgency item-data)
  (data:new-property item-data #:key base-urgency-key #:name "Base Urgency" #:default (cons 'number 0)))

(define (get-base-urgency item-data item)
  (data:get-property item-data item base-urgency-key))

(define (set-base-urgency item-data item base-urgency)
  (data:set-property item-data item base-urgency-key base-urgency))

(define (urgency item-data item)
  (get-base-urgency item-data item))

;; Takes item-data and a set of items, returns sorted list of items.
(define (sort-items-by-urgency-descending item-data item-set)
  (sort (set->list item-set) >=
        #:key (λ (item)
                (urgency item-data item))))
