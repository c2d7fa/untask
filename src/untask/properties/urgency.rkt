#lang racket

(provide urgency-property
         sort-items-by-urgency-descending)

(require
 (prefix-in i: "../core/item.rkt")
 (prefix-in p: "../core/property.rkt")
 (prefix-in val: "../core/value.rkt")
 "../../squiggle.rkt"

 (prefix-in depends: "./dependencies.rkt")
 (prefix-in status: "./status.rkt"))

(define (calculate-urgency item-state item)
  (val:make-number (+ (val:unwrap-number (or (i:get item-state item 'urgency) (val:make-number 0)))
                      (apply max 0
                             (map (λ (blo)
                                    (val:unwrap-number (calculate-urgency item-state (val:unwrap-item blo))))
                                  (filter (λ (blo)
                                            (not (status:done? item-state (val:unwrap-item blo))))
                                          (set->list (val:unwrap-set (p:get item-state item depends:blocks-property)))))))))

(define urgency-property
  (p:property #:name 'urgency
              #:type 'number
              #:calculate calculate-urgency))

;; Takes item state and a set of items, returns sorted list of items.
(define (sort-items-by-urgency-descending item-state item-set)
  (define (number>= x y)
    (>= (val:unwrap-number x) (val:unwrap-number y)))
  (sort (set->list item-set) number>=
        #:key (λ (item)
                (calculate-urgency item-state item))))
