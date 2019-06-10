#lang racket

(provide (all-defined-out))

(require
  (prefix-in prop: "../core/property.rkt")
  (prefix-in val: "../core/value.rkt"))

(define tags-property-type
  (prop:make-property-type #:key 'tags
                           #:type 'set
                           #:default (val:make-set)))
