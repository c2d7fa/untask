#lang racket

(provide
 depends-property-type
 blocks-property-type)

(require
  (prefix-in item: "../data/item-data.rkt")
  (prefix-in prop: "../data/property-type.rkt")
  (prefix-in val: "../data/values.rkt"))

(define depends-property-type
  (prop:make-property-type #:key 'depends
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
  (error "unimplemented"))

(define blocks-property-type
  (prop:make-property-type #:key 'blocks
                           #:calculate calculate-blocks
                           #:translate translate-blocks))
