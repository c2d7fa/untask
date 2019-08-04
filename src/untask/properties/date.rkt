#lang racket

(provide wait-property-type
         date-property-type
         wait-active?)

(require
 (prefix-in item: "../core/item.rkt")
 (prefix-in prop: "../core/property.rkt")
 (prefix-in val:  "../core/value.rkt")

 (prefix-in dt: "../../datetime.rkt"))

(define wait-property-type
  (prop:make-property-type #:key 'wait
                           #:type '(opt date)
                           #:default #f))

(define date-property-type
  (prop:make-property-type #:key 'date
                           #:type '(opt date)
                           #:default #f))

(define (wait-active? item-data item)
  (let ((prop (item:get-property item-data item wait-property-type)))
    (or (not prop)
        (not (dt:future? (val:unwrap-date prop))))))
