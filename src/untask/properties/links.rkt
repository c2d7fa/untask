#lang racket

(provide children-property parents-property)

(require
 (prefix-in i: "../core/item.rkt")
 (prefix-in p: "../core/property.rkt")
 (prefix-in val: "../core/value.rkt")
 (only-in "../../misc.rkt" set-diff)
 "../../squiggle.rkt")

(define children-property
  (~> (p:property #:name 'children
                  #:type '(set item))
      (p:default (val:make-set))))

;; TODO: The following is mostly copy-pasted from ./dependencies.rkt. We could
;; probably extract this code to its own function.

(define (calculate-parents item-state item)
  (val:make-set
   (list->set
    (map val:make-item
         (filter (λ (item*)
                   (set-member? (set-map (val:unwrap-set (p:get item-state item* children-property))
                                         val:unwrap-item)
                                item))
                 (set->list (i:items item-state)))))))

(define (translate-parents item-state item value)
  (define old-parents (p:get item-state item parents-property))
  (define-values (removed-parents added-parents)
    (set-diff (val:unwrap-set old-parents)
              (val:unwrap-set value)))
  (define item-state-with-items-removed
    (foldl (λ (removed-blocked-item item-state)
             (p:update item-state
                       (val:unwrap-item removed-blocked-item)
                       children-property
                       (λ (children-value)
                         (val:make-set (set-remove (val:unwrap-set children-value) (val:make-item item))))))
           item-state
           (set->list removed-parents)))
  (foldl (λ (added-blocked-item item-state)
           (p:update item-state
                     (val:unwrap-item added-blocked-item)
                     children-property
                     (λ (children-value)
                       (val:make-set (set-add (val:unwrap-set children-value)
                                              (val:make-item item))))))
           item-state-with-items-removed
           (set->list added-parents)))

(define parents-property
  (p:property #:name 'parents
              #:type '(set any)
              #:calculate calculate-parents
              #:translate translate-parents))
