#lang racket

(provide calculate-backlinks translate-backlinks)

(require
 (prefix-in i: "../core/item.rkt")
 (prefix-in p: "../core/property.rkt")
 (prefix-in val: "../core/value.rkt")
 (only-in "../../misc.rkt" set-diff))

(define ((calculate-backlinks property) item-state item)
  (val:make-set
   (list->set
    (map val:make-item
         (filter (λ (item*)
                   (set-member? (set-map (val:unwrap-set (p:get item-state item* property))
                                         val:unwrap-item)
                                item))
                 (set->list (i:items item-state)))))))

(define ((translate-backlinks property) item-state item value)
  (define old-value ((calculate-backlinks property) item-state item))
  (define-values (removed-items added-items)
    (set-diff (val:unwrap-set old-value)
              (val:unwrap-set value)))
  (define item-state-with-items-removed
    (foldl (λ (removed-item item-state)
             (p:update item-state
                       (val:unwrap-item removed-item)
                       property
                       (λ (items)
                         (val:make-set (set-remove (val:unwrap-set items) (val:make-item item))))))
           item-state
           (set->list removed-items)))
  (foldl (λ (added-item item-state)
           (p:update item-state
                     (val:unwrap-item added-item)
                     property
                     (λ (items)
                       (val:make-set (set-add (val:unwrap-set items) (val:make-item item))))))
         item-state-with-items-removed
         (set->list added-items)))
