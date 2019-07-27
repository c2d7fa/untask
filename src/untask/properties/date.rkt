#lang racket

(provide wait-property-type)

(require
 (prefix-in item: "../core/item.rkt")
 (prefix-in prop: "../core/property.rkt")
 (prefix-in val:  "../core/value.rkt"))

(define wait-property-type
  (prop:make-property-type #:key 'wait
                           #:type '(opt date)
                           #:default #f))
