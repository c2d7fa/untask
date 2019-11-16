#lang racket

(provide translate-links translate-backlinks)

(require
 (prefix-in i: untask/src/untask/core/item)
 (prefix-in p: untask/src/untask/core/property)
 (prefix-in val: untask/src/untask/core/value)
 (only-in untask/src/misc set-diff))

(define (unwrap-set-or-empty val-set)
  (val:unwrap-set (or val-set (val:make-set (set)))))

;; TODO: Why are we using two completely different implementations for
;; translate-links and translate-backlinks? Shouldn't these be symmetrical?

(define ((translate-links links-property-key backlinks-property-key) item-state item value)
  (define old-value (i:get item-state item links-property-key))
  (define (calculate-backlinks item-state item)
    (val:make-set
      (list->set
       (map val:make-item
            (filter (λ (item*)
                      (set-member? (set-map (unwrap-set-or-empty (i:get item-state item* links-property-key))
                                            val:unwrap-item)
                                   item))
                    (set->list (i:items item-state)))))))
  (foldl (λ (linked-item item-state)
           (i:set item-state linked-item backlinks-property-key (calculate-backlinks item-state linked-item)))
         (i:set item-state item links-property-key value)
         (map val:unwrap-item (set->list (set-union (unwrap-set-or-empty old-value) (unwrap-set-or-empty value))))))

(define ((translate-backlinks links-property-key backlinks-property-key) item-state item value)
  (define old-value (i:get item-state item backlinks-property-key))
  (define-values (removed-items added-items)
    (set-diff (unwrap-set-or-empty old-value)
              (unwrap-set-or-empty value)))
  (define item-state-with-backlinks-updated
    (i:set item-state item backlinks-property-key value))
  (define item-state-with-items-removed
    (foldl (λ (removed-item item-state)
             (i:update item-state
                       (val:unwrap-item removed-item)
                       links-property-key
                       (λ (items)
                         (val:make-set (set-remove (unwrap-set-or-empty items) (val:make-item item))))))
           item-state-with-backlinks-updated
           (set->list removed-items)))
  (foldl (λ (added-item item-state)
           (i:update item-state
                     (val:unwrap-item added-item)
                     links-property-key
                     (λ (items)
                       (val:make-set (set-add (unwrap-set-or-empty items) (val:make-item item))))))
         item-state-with-items-removed
         (set->list added-items)))
