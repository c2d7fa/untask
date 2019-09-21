#lang racket

(provide children-property-type parents-property-type)

(require
 (prefix-in item: "../core/item.rkt")
 (prefix-in prop: "../core/property.rkt")
 (prefix-in val: "../core/value.rkt")

 (only-in "../../misc.rkt" set-diff))

(define children-property-type
  (prop:property-type #:key 'children
                      #:type '(set item)
                      #:default (val:make-set)))

;; TODO: The following is mostly copy-pasted from ./dependencies.rkt. We could
;; probably extract this code to its own function.

(define (calculate-parents item-data item)
  (val:make-set
   (list->set
    (map val:make-item
         (filter (λ (im)
                   (set-member? (set-map (val:unwrap-set (item:get-property item-data im children-property-type))
                                         val:unwrap-item)
                                item))
                 (set->list (item:all-items item-data)))))))

(define (translate-parents item-data item value)
  (define old-parents
    (item:get-property item-data item parents-property-type))
  (define-values (removed-parents added-parents)
    (set-diff (val:unwrap-set old-parents)
              (val:unwrap-set value)))
  (define item-data-with-items-removed
    (foldl (λ (removed-blocked-item item-data)
             (item:update-property item-data
                                   (val:unwrap-item removed-blocked-item)
                                   children-property-type
                                   (λ (children-value)
                                     (val:make-set (set-remove (val:unwrap-set children-value) (val:make-item item))))))
           item-data
           (set->list removed-parents)))
  (foldl (λ (added-blocked-item item-data)
           (item:update-property item-data
                                 (val:unwrap-item added-blocked-item)
                                 children-property-type
                                 (λ (children-value)
                                   (val:make-set (set-add (val:unwrap-set children-value)
                                                          (val:make-item item))))))
           item-data-with-items-removed
           (set->list added-parents)))

(define parents-property-type
  (prop:property-type #:key 'parents
                      #:type '(set any)
                      #:calculate calculate-parents
                      #:translate translate-parents))
