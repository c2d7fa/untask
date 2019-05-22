#lang racket

(provide (all-defined-out))

(require
  (prefix-in prop: "../data/property-type.rkt")
  (prefix-in val: "../data/values.rkt"))

(define base-urgency-property-type
  (prop:make-property-type #:key 'baseurgency
                           #:default (val:make-number 0)))
