#lang racket

(provide (all-defined-out))

(require
  (prefix-in item: "../data/item-data.rkt")
  (prefix-in prop: "../data/property-type.rkt")
  (prefix-in val: "../data/values.rkt")
  (prefix-in depends: "./dependencies.rkt"))

(define base-urgency-property-type
  (prop:make-property-type #:key 'baseurgency
                           #:default (val:make-number 0)))

(define (calculate-urgency item-data item)
  (val:make-number (+ (val:unwrap-number (item:get-property item-data item base-urgency-property-type))
                      (apply max 0
                             (map (λ (blo)
                                    (val:unwrap-number (calculate-urgency item-data (val:unwrap-item blo))))
                                  (set->list (val:unwrap-set (item:get-property item-data item depends:blocks-property-type))))))))

(define (translate-urgency item-data item value)
  (item:set-property item-data item base-urgency-property-type value))

(define urgency-property-type
  (prop:make-property-type #:key 'urgency
                           #:calculate calculate-urgency
                           #:translate translate-urgency))

;; Takes item-data and a set of items, returns sorted list of items.
(define (sort-items-by-urgency-descending item-data item-set)
  (define (number>= x y)
    (>= (val:unwrap-number x) (val:unwrap-number y)))
  (sort (set->list item-set) number>=
        #:key (λ (item)
                (calculate-urgency item-data item))))
