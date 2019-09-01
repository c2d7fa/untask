#lang racket

(provide (all-defined-out))

(require
  (prefix-in prop: "../core/property.rkt")
  (prefix-in val: "../core/value.rkt"))

(define tags-property-type
  (prop:property-type #:key 'tags
                      #:type '(set string)
                      #:default (val:make-set)))
