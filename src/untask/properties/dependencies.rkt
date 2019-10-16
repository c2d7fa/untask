#lang racket

(provide depends-property
         blocks-property)

(require
 (prefix-in i: "../core/item.rkt")
 (prefix-in p: "../core/property.rkt")
 (prefix-in val: "../core/value.rkt")
 (only-in "../../misc.rkt" set-diff)
 "../../squiggle.rkt")

(define depends-property
  (~> (p:property #:name 'depends
                  #:type '(set item))
      (p:default (val:make-set))))

(define (calculate-blocks item-state item)
  (val:make-set
   (list->set
    (map val:make-item
         (filter (λ (item)
                   (set-member? (set-map (val:unwrap-set (p:get item-state item depends-property))
                                         val:unwrap-item)
                                item))
                 (set->list (i:items item-state)))))))

(define (translate-blocks item-state item value)
  (define old-blocks (p:get item-state item blocks-property))
  (define-values (removed-blocks added-blocks)
    (set-diff (val:unwrap-set old-blocks)
              (val:unwrap-set value)))
  (define item-state-with-items-removed
    (foldl (λ (removed-blocked-item item-state)
             (p:update item-state
                       (val:unwrap-item removed-blocked-item)
                       depends-property
                       (λ (depends-value)
                         (val:make-set (set-remove (val:unwrap-set depends-value) (val:make-item item))))))
           item-state
           (set->list removed-blocks)))
  (foldl (λ (added-blocked-item item-state)
           (p:update item-state
                     (val:unwrap-item added-blocked-item)
                     depends-property
                     (λ (depends-value)
                       (val:make-set (set-add (val:unwrap-set depends-value)
                                              (val:make-item item))))))
           item-state-with-items-removed
           (set->list added-blocks)))

(define blocks-property
  (p:property #:name 'blocks
              #:type '(set any)
              #:calculate calculate-blocks
              #:translate translate-blocks))
