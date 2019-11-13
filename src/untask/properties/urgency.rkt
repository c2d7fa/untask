#lang racket

(provide urgency-property
         sort-items-by-urgency-descending)

(require
 (prefix-in i: untask/src/untask/core/item)
 (prefix-in p: untask/src/untask/core/property)
 (prefix-in val: untask/src/untask/core/value)
 untask/src/squiggle

 (prefix-in depends: untask/src/untask/properties/dependencies)
 (prefix-in status: untask/src/untask/properties/status))

(define (calculate-urgency item-state item)
  (val:make-number (+ (val:unwrap-number (or (i:get item-state item 'urgency) (val:make-number 0)))
                      (apply max 0
                             (map (λ (blo)
                                    (val:unwrap-number (calculate-urgency item-state (val:unwrap-item blo))))
                                  (filter (λ (blo)
                                            (not (status:done? item-state (val:unwrap-item blo))))
                                          (set->list (val:unwrap-set (p:get item-state item depends:blocks-property)))))))))

(define (translate-urgency item-state item value)
  (i:set item-state
         item
         'urgency
         (val:make-number (+ (val:unwrap-number (or (i:get item-state item 'urgency) (val:make-number 0)))
                             (- (val:unwrap-number value)
                                (val:unwrap-number (calculate-urgency item-state item)))))))

(define urgency-property
  (p:property #:name 'urgency
              #:type 'number
              #:calculate calculate-urgency
              #:translate translate-urgency))

;; Takes item state and a set of items, returns sorted list of items.
(define (sort-items-by-urgency-descending item-state item-set)
  (define (number>= x y)
    (>= (val:unwrap-number x) (val:unwrap-number y)))
  (sort (set->list item-set) number>=
        #:key (λ (item)
                (calculate-urgency item-state item))))
