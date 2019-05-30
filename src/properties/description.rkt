#lang racket

(provide (all-defined-out))

(require
 (prefix-in data: "../data/item-data.rkt")
 (prefix-in prop: "../data/property-type.rkt")
 (prefix-in val: "../data/values.rkt"))

(define description-property-type
  (prop:make-property-type #:key 'description
                           #:default (val:make-string "")))
