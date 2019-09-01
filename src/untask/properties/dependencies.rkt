#lang racket

(provide
 depends-property-type
 blocks-property-type)

(require
  (prefix-in item: "../core/item.rkt")
  (prefix-in prop: "../core/property.rkt")
  (prefix-in val: "../core/value.rkt")

  (only-in "../../misc.rkt" set-diff))

(define depends-property-type
  (prop:property-type #:key 'depends
                      #:type '(set item)
                      #:default (val:make-set)))

(define (calculate-blocks item-data item)
  (val:make-set
   (list->set
    (map val:make-item
         (filter (λ (im)
                   (set-member? (set-map (val:unwrap-set (item:get-property item-data im depends-property-type))
                                         val:unwrap-item)
                                item))
                 (set->list (item:all-items item-data)))))))

(define (translate-blocks item-data item value)
  (define old-blocks
    (item:get-property item-data item blocks-property-type))
  (define-values (removed-blocks added-blocks)
    (set-diff (val:unwrap-set old-blocks)
              (val:unwrap-set value)))
  (define item-data-with-items-removed
    (foldl (λ (removed-blocked-item item-data)
             (item:update-property item-data
                                   (val:unwrap-item removed-blocked-item)
                                   depends-property-type
                                   (λ (depends-value)
                                     (val:make-set (set-remove (val:unwrap-set depends-value) (val:make-item item))))))
           item-data
           (set->list removed-blocks)))
  (foldl (λ (added-blocked-item item-data)
           (item:update-property item-data
                                 (val:unwrap-item added-blocked-item)
                                 depends-property-type
                                 (λ (depends-value)
                                   (val:make-set (set-add (val:unwrap-set depends-value)
                                                          (val:make-item item))))))
           item-data-with-items-removed
           (set->list added-blocks)))

(define blocks-property-type
  (prop:property-type #:key 'blocks
                      #:type '(set any)
                      #:calculate calculate-blocks
                      #:translate translate-blocks))
