#lang racket

(provide (all-defined-out))

(require
  (prefix-in prop: "../data/property-type.rkt")
  (prefix-in val: "../data/values.rkt"))

(define tags-property-type
  (prop:make-property-type #:key 'tags
                           #:default (val:make-set)))
